#lang scribble/manual

@(require (for-label racket/base racket/contract evm-redex/pbt))

@title[#:tag "tutorial-pt"]{Tutorial: testando contratos Solidity com evm-redex}
@author{rodrigo}

@margin-note{English version: @other-doc['(lib "evm-redex/tutorial/tutorial-en.scrbl")].}

Este tutorial percorre, do zero, @bold{escrever um pequeno contrato em Solidity,
compilá-lo e testá-lo} com @racketmodname[evm-redex/pbt] — a DSL de testes
baseados em propriedades. Todos os trechos abaixo são o código real e executável
em @filepath{tutorial/} — @filepath{counter.rkt}, @filepath{token.rkt},
@filepath{ballot.rkt}, @filepath{auction.rkt}, @filepath{purchase.rkt} e
@filepath{explore.rkt}; rode todos com @exec{raco test tutorial}.

Você vai precisar do @tt{solc} (o compilador Solidity) para transformar um arquivo
@tt{.sol} no artefato JSON que a biblioteca lê; os artefatos versionados em
@filepath{tutorial/} foram gerados com @tt{solc 0.8.33}, então dá para acompanhar
sem recompilar.

@table-of-contents[]

@section{A ideia em um parágrafo}

A biblioteca executa o @emph{bytecode} do contrato sobre uma especificação
executável da EVM. Então o ciclo é sempre o mesmo: compile o @tt{.sol} para
bytecode com o @tt{solc}, carregue o artefato com @racket[read-artifact], faça o
@racket[deploy] num mundo novo e, então, ou chame o contrato com @racket[call]
(para ler estado ou enviar uma mensagem), ou enuncie uma @deftech{propriedade} com
@racket[define-evm-property] e deixe o motor tentar refutá-la sobre muitas entradas
geradas.

@section{Um primeiro contrato: @tt{Counter}}

Eis o "olá mundo" dos contratos com estado — um contador, em
@filepath{tutorial/contracts/Counter.sol}:

@filebox["Counter.sol"]{
@verbatim|{
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Counter {
    uint256 public count;                    // public -> getter `count()` de graça

    function increment() public { count += 1; }
    function add(uint256 n) public { count += n; }
    function decrement() public {
        require(count > 0, "underflow");     // uma guarda para testar reversão
        count -= 1;
    }
}
}|
}

@subsection{Compile}

Peça ao @tt{solc} o bytecode e a ABI, num único arquivo JSON:

@commandline{solc --combined-json bin,bin-runtime,abi contracts/Counter.sol > Counter.json}

@subsection{Carregue e faça o deploy}

@racket[read-artifact] extrai o bytecode de criação e de runtime (e a ABI) do
JSON; @racket[deploy] roda o construtor e devolve o endereço implantado e o mundo
resultante.

@racketblock[
(require evm-redex/pbt)

(define ART (read-artifact "Counter.json" #:contract "Counter"))

(define DEPLOYER #x00000000000000000000000000000000A11CE001)
(define DEP     (deploy (artifact-creation ART) #:from DEPLOYER))
(define COUNTER (deploy-result-address DEP))
(define BASE    (deploy-result-world DEP))   (code:comment "o mundo logo após o deploy")
]

@subsection{Leia o estado}

@tt{count} é uma variável pública, então o Solidity gera um getter @tt{count()}.
Um @racket[call] executa uma mensagem; @racket[call-result-return] são os bytes de
retorno, que @racket[abi-decode] converte de volta num número:

@racketblock[
(define (count-of world)
  (car (abi-decode (list "uint256")
                   (call-result-return
                    (call world COUNTER #:data (encode-call "count()"))))))

(count-of BASE)   (code:comment "=> 0")
]

@subsection{Envie transações}

@racket[encode-call] monta a calldata de uma função; @racket[call] devolve um
@racket[call-result], e @racket[call-result-world] é o mundo após a chamada.
Encadeie esse mundo por algumas chamadas e leia o contador de volta:

@racketblock[
(define CALLER #x00000000000000000000000000000000B0B00001)
(define (send world sig . args)
  (call-result-world
   (call world COUNTER #:from CALLER #:data (apply encode-call sig args))))

(let* ([w (send BASE "increment()")]
       [w (send w "add(uint256)" 5)])
  (count-of w))    (code:comment "=> 6")
]

@subsection{Enuncie uma propriedade}

Chamadas concretas servem para uma conferência rápida, mas o objetivo da
biblioteca é testar uma @tech{propriedade} sobre @emph{muitas} entradas geradas.
@racket[define-evm-property] no @emph{modo transação} (a cláusula @racket[#:call])
roda uma transação real; @racket[#:given] lista as entradas geradas, e
@racket[#:post] recebe o mundo antes (@racket[w0]) e depois (@racket[w1]) e o
resultado da transação:

@racketblock[
(define (tx sig . args)
  (make-tx #:sender CALLER #:to COUNTER #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property add-raises-count-by-n
  #:given ([n (gen-word-in 0 (expt 2 200))])
  #:world BASE
  #:call  (tx "add(uint256)" n)
  #:post  (lambda (w0 w1 r) (= (count-of w1) (+ (count-of w0) n))))

(check-evm-property add-raises-count-by-n #:trials 50)
]

Ao rodar, imprime:

@verbatim|{
  ✓ property add-raises-count-by-n passed 50 tests.
}|

Uma propriedade também pode exigir que uma chamada @emph{reverta}.
@racket[#:revert-when] diz "sob esta condição, a transação deve reverter"; a partir
de um contador zero, @tt{decrement()} sempre reverte:

@racketblock[
(define-evm-property decrement-reverts-at-zero
  #:given ()                     (code:comment "sem entradas geradas — uma asserção simples")
  #:world BASE                   (code:comment "aqui count é 0")
  #:call  (tx "decrement()")
  #:revert-when #t)

(check-evm-property decrement-reverts-at-zero #:trials 1)
]

@section{Um contrato mais rico: @tt{Token}}

Um token mínimo acrescenta @emph{saldos}, uma transferência @emph{guardada} e um
@emph{evento} — @filepath{tutorial/contracts/Token.sol}:

@filebox["Token.sol"]{
@verbatim|{
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Token {
    mapping(address => uint256) public balanceOf;   // -> `balanceOf(address)`
    uint256 public totalSupply;

    event Transfer(address indexed from, address indexed to, uint256 value);

    function mint(address to, uint256 amount) public {
        totalSupply += amount;
        balanceOf[to] += amount;
        emit Transfer(address(0), to, amount);
    }

    function transfer(address to, uint256 amount) public returns (bool) {
        require(balanceOf[msg.sender] >= amount, "insufficient balance");
        balanceOf[msg.sender] -= amount;
        balanceOf[to] += amount;
        emit Transfer(msg.sender, to, amount);
        return true;
    }
}
}|
}

Faça o deploy e, desta vez, construa um mundo em que @tt{ALICE} já tem 1000 tokens
(aqui o @tt{mint} é irrestrito, então qualquer conta pode chamá-lo):

@racketblock[
(define ART   (read-artifact "Token.json" #:contract "Token"))
(define ALICE #x000000000000000000000000000000000000A11CE)
(define BOB   #x0000000000000000000000000000000000000B0B00)

(define DEP    (deploy (artifact-creation ART) #:from #x00000000000000000000000000000000DEB00001))
(define TOKEN  (deploy-result-address DEP))

(define (u256 r) (car (abi-decode (list "uint256") (call-result-return r))))
(define (bal w a) (u256 (call w TOKEN #:data (encode-call "balanceOf(address)" a))))
(define (total w) (u256 (call w TOKEN #:data (encode-call "totalSupply()"))))

(define (send w from sig . args)
  (call-result-world (call w TOKEN #:from from #:data (apply encode-call sig args))))

(define MINTED (send (deploy-result-world DEP) ALICE "mint(address,uint256)" ALICE 1000))
(bal MINTED ALICE)   (code:comment "=> 1000")
]

@subsection{A propriedade que importa: conservação}

O invariante que um token nunca pode quebrar é que uma transferência @emph{move}
valor sem criar nem destruir nenhum. Esta única propriedade cobre tanto o caminho
de sucesso quanto o de reversão, sobre valores que cercam o saldo de ALICE:

@racketblock[
(define (tx from sig . args)
  (make-tx #:sender from #:to TOKEN #:gas-limit 200000 #:gas-price 0
           #:data (apply encode-call sig args)))

(define-evm-property transfer-conserves-supply
  #:given ([amount (gen-word-in 0 2000)])       (code:comment "cerca o saldo de 1000")
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:post  (lambda (w0 w1 r)
            (and (= (total w1) (total w0))       (code:comment "o supply nunca muda")
                 (if (eq? (txn-run-outcome r) 'revert)
                     (= (bal w1 ALICE) (bal w0 ALICE))          (code:comment "revertido")
                     (and (= (bal w1 ALICE) (- (bal w0 ALICE) amount))
                          (= (bal w1 BOB)   (+ (bal w0 BOB) amount)))))))

(check-evm-property transfer-conserves-supply #:trials 100)
]

E uma propriedade focada em reversão — transferir mais do que se tem deve falhar:

@racketblock[
(define-evm-property transfer-reverts-when-insufficient
  #:given ([amount (gen-word-in 1001 100000)])  (code:comment "sempre mais do que ALICE tem")
  #:world MINTED
  #:call  (tx ALICE "transfer(address,uint256)" BOB amount)
  #:revert-when #t)

(check-evm-property transfer-reverts-when-insufficient #:trials 50)
]

@margin-note{Quando uma propriedade @emph{falha}, o motor encolhe as entradas
aleatórias até um contraexemplo mínimo e o imprime — esse caso encolhido é o
grande ganho dos testes baseados em propriedades. Experimente enfraquecer o
@tt{require} de @tt{transfer} no contrato e rodar de novo: a propriedade de
conservação é refutada com uma transferência minúscula.}

@section{Mais exemplos do @emph{Solidity by Example}}

Os dois contratos acima já cobrem o ciclo inteiro; o resto desta seção o aplica
aos contratos clássicos da documentação do Solidity,
@hyperlink["https://docs.soliditylang.org/en/v0.8.36/solidity-by-example.html"]{Solidity
by Example}. Cada um é um módulo executável completo em @filepath{tutorial/} — a
fonte Solidity, o deploy e a propriedade que fixa sua regra central.

@subsection{Votação (Voting)}

@filepath{tutorial/ballot.rkt} testa @tt{Ballot}, o contrato de votação com
delegação. Duas coisas são novas aqui: o @bold{construtor recebe argumentos} (um
array de nomes de proposta em @tt{bytes32}), que você codifica em ABI e anexa ao
bytecode de criação; e um getter pode devolver uma @bold{struct}, que você
decodifica como uma tupla.

@racketblock[
(define ART (read-artifact "Ballot.json" #:contract "Ballot"))
(define CHAIR #x00000000000000000000000000000000C4A19001)

(code:comment "nomes de proposta são bytes32 — preencha cada um até 32 bytes")
(define NAMES (list (name->b32 "alpha") (name->b32 "beta") (name->b32 "gamma")))

(code:comment "args do construtor são codificados em ABI e anexados ao creation code")
(define DEP    (deploy (append (artifact-creation ART)
                               (abi-encode (list "bytes32[]") (list NAMES)))
                       #:from CHAIR))
(define BALLOT (deploy-result-address DEP))
]

O chairperson (quem faz o deploy) concede o direito de voto, o eleitor vota e a
contagem se atualiza — e a regra que vale checar é que @bold{só o chairperson pode
conceder direito de voto}, então uma chamada de qualquer outro deve reverter:

@racketblock[
(define OUTSIDER #x00000000000000000000000000000000BADA55001)
(define-evm-property only-chair-grants-rights
  #:given ([who gen-address])
  #:world BASE
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to BALLOT #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "giveRightToVote(address)" who))
  #:revert-when #t)                 (code:comment "quem não é chairperson sempre reverte")

(check-evm-property only-chair-grants-rights #:trials 30)
]

@subsection{Um leilão aberto}

@filepath{tutorial/auction.rkt} testa @tt{SimpleAuction}. Suas chamadas carregam
@bold{ether}: o @racket[#:value] de @racket[make-tx] financia o lance. Contas que
dão lances precisam de saldo, então enchemos algumas à mão — o mesmo que
@racket[deploy] faz por quem faz o deploy:

@racketblock[
(require (only-in evm-redex world-ref world-set acct-balance acct-with-balance))
(define (fund w a wei)
  (world-set w a (acct-with-balance (world-ref w a) (+ (acct-balance (world-ref w a)) wei))))
]

A regra central do leilão é que um lance é aceito @bold{exatamente quando} supera o
maior atual, e então se torna o novo maior — uma propriedade cobre tanto o caminho
de aceitação quanto o de rejeição:

@racketblock[
(define-evm-property bid-raises-the-highest
  #:given ([v (gen-word-in 0 500)])
  #:world W1                          (code:comment "aqui o maior lance é 100")
  #:call  (make-tx #:sender BOB #:nonce (nonce-of W1 BOB) #:to AUCTION #:value v
                   #:gas-limit 300000 #:gas-price 0 #:data (encode-call "bid()"))
  #:post  (lambda (w0 w1 r)
            (if (> v (highest-bid w0))
                (and (eq? (txn-run-outcome r) 'success) (= (highest-bid w1) v))
                (eq? (txn-run-outcome r) 'revert))))

(check-evm-property bid-raises-the-highest #:trials 60)
]

@subsection{Compra remota segura}

@filepath{tutorial/purchase.rkt} testa @tt{Purchase}, um escrow de quatro estados
(@tt{Created → Locked → Release → Inactive}). O @bold{construtor é payable} — o
vendedor tranca o dobro do valor do item — então você faz o deploy com
@racket[#:value]:

@racketblock[
(define DEP (deploy (artifact-creation ART) #:from SELLER #:value 200))  (code:comment "value = 100")
]

O caminho feliz é o comprador chamar @tt{confirmPurchase} (igualando o depósito) e
depois @tt{confirmReceived}. A regra que vale fixar é que, a partir do estado
@tt{Locked}, @bold{só o comprador} pode confirmar o recebimento:

@racketblock[
(define-evm-property only-buyer-confirms-receipt
  #:given ()
  #:world LOCKED-W
  #:call  (make-tx #:sender OUTSIDER #:nonce 0 #:to PURCHASE #:gas-limit 300000 #:gas-price 0
                   #:data (encode-call "confirmReceived()"))
  #:revert-when #t)

(check-evm-property only-buyer-confirms-receipt #:trials 1)
]

@margin-note{O quarto contrato do @emph{Solidity by Example}, o @bold{Canal de
Micropagamento}, @emph{não} está incluído: ele verifica uma assinatura ECDSA
off-chain com @tt{ecrecover}, e produzir essa assinatura acontece @emph{fora} da
EVM que esta biblioteca modela — então testá-lo exigiria um assinador off-chain,
além do escopo deste tutorial.}

@section{Explorando à mão com @tt{#lang evm-redex/sim}}

Propriedades servem para @emph{verificar}; quando você só quer @emph{cutucar} um
contrato, o simulador de transações se lê como um roteiro.
@filepath{tutorial/explore.rkt} é uma sessão inteira — rode com
@exec{racket tutorial/explore.rkt}:

@filebox["explore.rkt"]{
@verbatim|{
#lang evm-redex/sim
.account ALICE balance=1eth
.account BOB
.deploy TOKEN from=ALICE code=@Token.json:Token

tx from=ALICE to=TOKEN sig="mint(address,uint256)" args=(ALICE, 1000)
tx from=ALICE to=TOKEN sig="transfer(address,uint256)" args=(BOB, 100)
tx from=BOB   to=TOKEN sig="transfer(address,uint256)" args=(ALICE, 5000)  ; reverte
call from=ALICE to=TOKEN sig="balanceOf(address)" args=(BOB) returns=uint256
}|
}

Ele imprime um recibo por transação — com o evento @tt{Transfer} e a razão da
reversão já decodificados — e um diff final do estado:

@verbatim|{
tx ALICE -> TOKEN
  status:    success
  log:       Transfer(from=0x0, to=0x4e8a…8f7, value=0x3e8)
tx ALICE -> TOKEN
  status:    success
  log:       Transfer(from=0x4e8a…8f7, to=0x28e4…4c38, value=0x64)
tx BOB -> TOKEN
  status:    revert  (Error("insufficient balance"))
call TOKEN.balanceOf(address) = (100)

--- final state ---
state root: 0x4b40…1730
…
}|

@section{Por onde seguir}

@itemlist[
@item{A referência completa de @racketmodname[evm-redex/pbt] — cada cláusula de
      @racket[define-evm-property], o vocabulário de observação e os geradores —
      está em @secref["pbt" #:doc '(lib "evm-redex/scribblings/evm-redex.scrbl")].}
@item{A linguagem de cenários @tt{#lang evm-redex/sim} está documentada em
      @secref["sim" #:doc '(lib "evm-redex/scribblings/evm-redex.scrbl")].}
@item{Exemplos trabalhados maiores — ERC-20/721 reais, contratos da OpenZeppelin e
      um benchmark que reproduz bugs reais de auditoria — vivem no pacote
      companheiro
      @hyperlink["https://github.com/lives-group/evm-redex-tests"]{@tt{evm-redex-tests}},
      que depende desta biblioteca.}
]
