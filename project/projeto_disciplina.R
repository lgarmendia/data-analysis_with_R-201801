# Componentes: Guaracy Dias, Guilherme Vivian e Lucas Garmendia

# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?

products %>%
    select(product_id) %>%
    pull() -> products_all

insta_products %>%
    select(product_id) %>%
    distinct() %>%
    arrange(product_id) %>%
    pull() -> products_orders

sum(!(products_all %in% products_orders))

#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

products %>%
    inner_join(aisles, by = "aisle_id") %>%
    inner_join(departments, by = "department_id") %>%
    select(product_id, product_name, aisle_id, aisle, department_id, department) -> products_hierarchy

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

products_hierarchy %>%
    group_by(aisle_id, aisle, department_id, department) %>%
    summarise(qty = n()) %>%
    ungroup() %>%
    arrange(desc(qty)) %>%
    head(10) -> hierarchy_top_10

#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

products %>%
    inner_join(hierarchy_top_10, by = c("aisle_id", "department_id")) %>%
    inner_join(insta_products, by = "product_id") %>%
    distinct(order_id) %>%
    summarise(n()) / count(insta_orders)

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

hierarchy_top_10 %>%
    filter(aisle != 'missing' & department != 'missing') %>%
    inner_join(products, by = c("aisle_id", "department_id")) %>%
    inner_join(insta_products, by = "product_id") %>%
    distinct(product_id, product_name, aisle_id, department_id) %>%
    select(product_id, product_name, aisle_id, department_id) -> products_categorized

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
# Transforme as variáveis user_id, department e aisle em factor
# Transforme a variável order_hour_of_day em um factor ordenado (ordered)

# Este dataframe deverá ser utilizado em todas as atividades seguintes

hierarchy_top_10 %>%
    select(aisle_id, department_id) -> hierarchy_top_10

products %>%
    inner_join(aisles, by = "aisle_id") %>%
    inner_join(departments, by = "department_id") %>%
    inner_join(hierarchy_top_10, by = c("aisle_id", "department_id")) %>%
    inner_join(insta_products, by = "product_id") %>%
    inner_join(insta_orders, by = "order_id") %>%
    select(
        product_id,
        product_name,
        aisle_id,
        aisle,
        department_id,
        department,
        order_id,
        user_id,
        order_number,
        order_dow,
        order_hour_of_day,
        days_since_prior_order) -> df_all

df_all %>%
    mutate(user_id = factor(user_id),
           aisle = factor(aisle),
           department = factor(department),
           order_hour_of_day = factor(order_hour_of_day, ordered = TRUE)) -> df_all

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos

df_all %>%
    distinct(order_hour_of_day, user_id) %>%
    group_by(order_hour_of_day) %>%
    summarise(qty = n()) %>%
    ungroup() %>%
    arrange(desc(qty)) %>%
    head(5) %>%
    pull(order_hour_of_day) -> order_hour_of_day_top_5

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

df_all %>%
    filter(order_hour_of_day %in% order_hour_of_day_top_5) %>%
    group_by(product_id, product_name) %>%
    summarise(qty = n()) %>%
    ungroup() %>%
    arrange(desc(qty)) %>%
    head(15) -> products_top_15

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
# e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
# Utilize o nome do produto para legenda da cor da linha.
# Você consegue identificar algum produto com padrão de venda diferente dos demais? 

df_all %>%
    inner_join(products_top_15, c = 'product_id') %>%
    mutate(qty = 1) %>%
    group_by(product_id, product_name, order_hour_of_day, order_dow) %>%
    summarise(total = sum(qty)) %>%
    ungroup() %>%
    group_by(product_id, product_name, order_hour_of_day) %>%
    summarise(sales_mean = mean(total)) %>%
    ungroup() %>%
    ggplot(aes(x = order_hour_of_day, y = sales_mean, group = product_name)) +
    geom_line(aes(color = product_name))

# Apesar de todos terem um padrão similar, o produto 'Organic Whole String Cheese' é o que mais teve volume de vendas.

#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
# Média, Desvio Padrão, Mediana, Mínimo e Máximo
# Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 

df_all %>%
    distinct(order_id, order_dow, order_hour_of_day) %>%
    mutate(qty = 1) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(total = sum(qty)) %>%
    group_by(order_hour_of_day) %>%
    summarise(mean(total), sd(total), median(total), min(total), max(total)) %>%
    ungroup()

# É Gaussiana, pois cresce até determinada hora e depois passa a cair.

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

df_all %>%
    mutate(qty = 1) %>%
    group_by(product_id, order_hour_of_day) %>%
    summarise(total = sum(qty)) %>%
    ungroup() %>%
    group_by(order_hour_of_day) %>%
    mutate(low = mean(total) - sd(total), high = mean(total) + sd(total)) %>%
    ungroup() %>%
    ggplot(aes(x = order_hour_of_day, y = total, ymin = low, ymax = high)) +
    geom_ribbon(fill = "lightgray", alpha = 0.5) +
    geom_jitter(alpha = .2, height = 0, width = 0.3) +
    theme_bw()


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.
df_all %>%
    mutate(qty = 1) %>%
    group_by(order_hour_of_day, order_dow) %>%
    summarise(total = sum(qty)) %>%
    ggplot( aes( x = order_dow, y = total, group = order_dow )) +
    geom_boxplot() +
    scale_x_continuous( breaks = 0:6 ) +
    labs( x = "Day"
          , y = "Orders") +
    theme_bw()


#13 # Identifique, por usuário, o tempo médio entre pedidos
df_all %>%
    group_by(user_id) %>%
    summarise(avg_time_orders = mean(days_since_prior_order)) %>%
    ungroup() %>%
    arrange(user_id)


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado
df_all %>%
    group_by(user_id) %>%
    summarise(avg_time_orders = mean(days_since_prior_order)) %>%
    ggplot( aes( x = avg_time_orders )) +
    geom_bar(fill="gray", alpha=0.6) +
    scale_x_continuous(breaks = 0:100) +
    theme_bw()


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 
df_all %>%
    group_by(user_id, days_since_prior_order) %>%
    ggplot( aes( x = days_since_prior_order )) +
    geom_bar(fill="gray", alpha=0.6) +
    scale_x_continuous(breaks = 0:100) +
    theme_bw()

#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?
df_all %>%
    group_by(user_id) %>%
    summarise(avg_time_orders = mean(days_since_prior_order)
              ,qty_orders = n()) %>%
    filter(qty_orders >= 5) %>%
    ggplot( aes( x = avg_time_orders )) +
    geom_bar(fill="gray", alpha=0.6) +
    scale_x_continuous(breaks = 0:100) +
    theme_bw()

#sim, o padrão se mantém

#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
# Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.

bananas <- c(13176, 24852, 29259, 37067, 39276)

#df_all %>% filter(product_id %in% bananas)

# Não existiram bananas no df_all (atividade 6), utilizou-se, então, o vetor original.

insta_products %>%
    filter(product_id %in% bananas) %>%
    group_by(order_id) %>%
    summarise(qty = n()) %>%
    ungroup() %>%
    filter(qty > 1) %>%
    pull(order_id) -> orders_bananas

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
# Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências

insta_products %>%
    filter(product_id %in% bananas & order_id %in% orders_bananas) %>%
    distinct(order_id, product_id) %>%
    group_by(product_id) %>%
    summarise(qty = n()) %>%
    arrange(desc(qty)) %>%
    head(3) %>%
    pull(product_id) -> bananas_top_3

#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana.

products %>%
    filter(product_id %in% bananas_top_3) %>%
    inner_join(insta_products, by = "product_id") %>%
    inner_join(insta_orders, by = "order_id") %>%
    distinct(order_dow, order_hour_of_day, order_id) %>%
    mutate(qty = 1) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(orders_mean = mean(sum(qty))) %>%
    ungroup()

#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
# e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
# nesta combinação de dia da semana com hora

products %>%
    filter(product_id %in% bananas_top_3) %>%
    inner_join(insta_products, by = "product_id") %>%
    inner_join(insta_orders, by = "order_id") %>%
    distinct(order_dow, order_hour_of_day, order_id) %>%
    ggplot(aes(x = order_dow, y = order_hour_of_day, color = ..n..)) +
    geom_count()

#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana

products %>%
    filter(product_id %in% bananas_top_3) %>%
    inner_join(insta_products, by = "product_id") %>%
    inner_join(insta_orders, by = "order_id") %>%
    distinct(order_dow, order_hour_of_day, order_id) %>%
    mutate(qty = 1) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(orders_mean = mean(sum(qty))) %>%
    ungroup() %>%
    ggplot(aes(x = orders_mean)) +
    scale_x_continuous(breaks = seq(from = 0, to = 850, by = 50)) +
    scale_y_continuous(breaks = seq(from = 0, to = 20, by = 1)) +
    geom_histogram(breaks = seq(from = 0, to = 850, by = 5)) +
    facet_wrap(~order_dow, ncol = 2)

#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

insta_orders %>%
    filter(order_dow %in% c(3, 4)) %>%
    mutate(qty = 1) %>%
    group_by(order_dow, order_hour_of_day) %>%
    summarise(total = sum(qty)) -> insta_orders_wilcoxon

wilcox.test(
    total ~ order_dow,
    data = insta_orders_wilcoxon
)