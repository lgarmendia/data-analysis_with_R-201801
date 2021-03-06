### Atividade prática

## Vamos começar carregando um ambiente previamente criado para esta aula. 
## Nas aulas seguintes trabalharemos com fontes de dados em arquivos de formatos diversos.
load("aula-02/data/dados_exercicio.RData")

### 1 ####
## Inicie mostrando uma prévia do conteúdo da variável acessos_alunos
## 
## Dica 1: No material sobre estruturas de dados vimos como exibir uma prévia do conteúdo de uma variável com 2 funções diferentes
## Dica 2: Na primeira aula vimos uma função do RStudio que permite visualizar o conteúdo de uma variável, mas neste caso 
##         quero ver uma saída na Console.
### # ####


str(acessos_alunos)




### 2 ###
## Quantos elementos a variável acessos_alunos possui? Utilize uma função do R que retorna o tamanho da variável.

## Dica: Vimos um exemplo no mesmo material sobre estruturas de dados
### # ###
#alu201830199
length(acessos_alunos)



### 3 ###
## Utilizando o seu código de aluno da Uniritter como nome de um valor da lista, imprima uma linha informando quantos acessos
## você fez. A linha deve ser impressa na Console, com um texto que diga o seu código de aluno e o valor conforme o seguinte exemplo:
## "O aluno <alu...> realizou N acessos."

## Dica 1: Utilize a função paste() para composição do texto que será impresso. 
## Dica 2: Vimos exemplos disto nos materiais dos tipos numéricos e das estruturas de dados.
### # ###
meuacesso <- acessos_alunos[["alu201830199"]]
meuacesso

paste("O aluno alu201830199 realizou", meuacesso, "acessos")


### 4 ###
## A operação abaixo cria um vetor com todas as quantidades de acessos por aluno.
acessos <- unlist(acessos_alunos)


## Após a criação deste vetor, determine quantos colegas fizeram mais acessos que você.
## Faça isso em 3 etapas: 
## 1. Crie uma variável com o resultado de um teste de comparação (relacional) entre o seu número de acessos e os demais.
## 2. Com uma operação de indexação, crie um outro vetor contendo somente os valores maiores
## 3. Determine o tamanho do vetor da operação 2, imprimindo o resultado na Console
### # ###

diferente <- acessos[acessos != meuacesso]

maior <- acessos[acessos > meuacesso]

length(maior)

### 5 ###
## Combine todas as etapas acima em uma única chamada, sem a criação dos vetores auxiliares
### # ###

length(acessos[acessos>meuacesso])


### 6 ###
## Agora determine quantos colegas fizeram menos acessos que você. 
## Faça isso utilizando a função sum!
menorvet <- acessos[acessos<meuacesso]

menor <- c(menorvet)

sum(menor < meuacesso)

## Dica: Lembre que falamos sobre como o R faz conversões implícitas entre o tipo lógico e tipos numéricos
### # ###


### 7 ###
## Supondo que eu quero atribuir uma nota de participação baseada na quantidade de acessos, com a seguinte definição:
##   - Alunos que não acessaram não recebem nota de participação
##   - Alunos que acessaram, mas menos que 10 vezes, recebem 1 ponto
##   - Alunos que acessaram 10 vezes ou mais recebem 2 pontos
## Crie um vetor chamado notas com a nota de cada aluno, na mesma ordem do vetor de acessos criado para o exercício 4.

## Dica: Pode ser mais fácil se iniciar o vetor notas como uma cópia do vetor acessos, modificando os valores conforme as regras
## OBSERVAÇÃO :: Não avaliarei participação na forma do enunciado deste exercício. 
### # ###
notas <- acessos

notas[which(notas == 0)] <- NA # row numbers

notas[which(notas > 0 & notas < 10)] <- 1

notas[which(notas >= 10)] <- 2

notas

### 8 ###
## Visualização da quantidade de alunos com cada nota de participação. Esta não é uma atividade, apenas uma ilustração de como
## criar uma tabela com esta contagem
table(notas)



### 9 ###
## Abaixo, criei uma versão modificada da lista acessos_alunos, com a inclusão de um acesso convidado.
## Não foi possível determinar o número de acessos por não existir um login para este tipo de acesso.
acessos_alunos_e_guest <- acessos_alunos
acessos_alunos_e_guest$guest <- NA
acessosguest <- unlist(acessos_alunos_e_guest)

## Repita as atividades 4, 5, 6, e 7 utilizando o acessos_com_guest no lugar da lista acessos_alunos.
## Tome o devido cuidado de sempre criar variáveis com nomes diferentes das já utilizadas! 
##Diferente
diferenteacesso <- acessosguest[acessosguest != meuacesso]
diferenteacesso
##maior
maiorguest <- acessosguest[acessosguest > meuacesso]
maiorguest
##tamanho
length(maiorguest)
##unico 
length(acessosguest[acessosguest>meuacesso])
##menor
menorvetguest <- acessosguest[acessosguest<meuacesso]
##soma menor acesso
sum(menorvetguest < meuacesso)
##exercicio 7
notasguest <- acessosguest

notasguest[which(notasguest == 0)] <- NA # row numbers

notasguest[which(notasguest > 0 & notasguest < 10)] <- 1

notasguest[which(notasguest >= 10)] <- 2

notasguest

### 10 ###
## Responda as seguintes perguntas:


# 1. Houve modificação no número de alunos com mais e com menos acessos que você?
# Sim
# 2. Como você conclui que o R trata comparações (operações relacionais) entre valores numéricos e NA?
# Ele considera o NA para fazer a comparação, mas não sabe identificar o tipo de valor e acaba trazendo o resultado para ambas comparações.
# 3. Qual o resultado do uso da função sum na presença de NA? O que você conclui sobre a operação de soma de todos os valores de
#    um vetor na presença de NA?
# Ele retornará NA se possuir alguma valor NA no vetor.
# 4. Execute o comando abaixo para ler a documentação da função sum e veja se há como modificar a chamada da função sum na presença
#    de NAs. Teste os exemplos da página de help da função sum.
#Sim é possível alterar a função SUM utlizando na.rm = TRUE
help(sum)
sum(menorvetguest < meuacesso, na.rm = TRUE)