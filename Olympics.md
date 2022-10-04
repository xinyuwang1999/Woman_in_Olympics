Women In the Olympics
================
Amy Wang
3/20/2022

## Introduction

Olympic games are the leading international sporting events featuring
summer and winter sports competitions in which thousands of athletes
from around the world participate in a variety of competitions. [^1]

This data set contains all the winter and summer Olympic Games from
Athens 1896 to Rio 2016, including 15 variables. Each row corresponds to
an individual athlete competing in an individual Olympic event. The 1896
Summer Olympics, was an international multi-sport event that was
celebrated in Athens, Greece, from 6 to 15 April 1896.[^2] It was the
first Olympic Games held in the Modern era. On January 25, 1924, the
first Winter Olympics take off in style at Chamonix in the French
Alps.[^3]

``` r
glimpse(olympics)
```

    ## Rows: 271,116
    ## Columns: 15
    ## $ id     <dbl> 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, …
    ## $ name   <chr> "A Dijiang", "A Lamusi", "Gunnar Nielsen Aaby", "Edgar Lindenau…
    ## $ sex    <chr> "M", "M", "M", "M", "F", "F", "F", "F", "F", "F", "M", "M", "M"…
    ## $ age    <dbl> 24, 23, 24, 34, 21, 21, 25, 25, 27, 27, 31, 31, 31, 31, 33, 33,…
    ## $ height <dbl> 180, 170, NA, NA, 185, 185, 185, 185, 185, 185, 188, 188, 188, …
    ## $ weight <dbl> 80, 60, NA, NA, 82, 82, 82, 82, 82, 82, 75, 75, 75, 75, 75, 75,…
    ## $ team   <chr> "China", "China", "Denmark", "Denmark/Sweden", "Netherlands", "…
    ## $ noc    <chr> "CHN", "CHN", "DEN", "DEN", "NED", "NED", "NED", "NED", "NED", …
    ## $ games  <chr> "1992 Summer", "2012 Summer", "1920 Summer", "1900 Summer", "19…
    ## $ year   <dbl> 1992, 2012, 1920, 1900, 1988, 1988, 1992, 1992, 1994, 1994, 199…
    ## $ season <chr> "Summer", "Summer", "Summer", "Summer", "Winter", "Winter", "Wi…
    ## $ city   <chr> "Barcelona", "London", "Antwerpen", "Paris", "Calgary", "Calgar…
    ## $ sport  <chr> "Basketball", "Judo", "Football", "Tug-Of-War", "Speed Skating"…
    ## $ event  <chr> "Basketball Men's Basketball", "Judo Men's Extra-Lightweight", …
    ## $ medal  <chr> NA, NA, NA, "Gold", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

In this data set, we can see that there is any data from 1916, 1940,
1944. Through research, I found that the 1916, 1940 & 1944 summer
Olympic games and the 1940 & 1944 winter Olympic games didn’t happen due
to the World Wars (World War I (1916) and World War II (1940, 1944)).
[^4]

The Olympic Games were founded in 1894 as a celebration of virility.
Women were admitted in 1900 as participants in sports that were
considered to be compatible with their femininity and fragility but were
excluded from the showpiece events of track and field. Although there
were an increasing amount of females who participated in the Olympics,
gender imbalance was dominant throughout the twentieth century,
including in the International Olympic Committee (IOC). In order to
provide more gender equality in Games, the Olympic charter has made the
presence of women mandatory in every sport since 2007. The IOC also
added gender parity to the 2020 Olympics agenda.[^5]

Since March is Women’s History Month, for this project, I will meanly
focus on female athletes and their achievements through the 120 years of
the Olympic Games’ history.

## What is the age range for female athelets?

``` r
female <- olympics %>% 
  filter(sex == "F") 

female1 <- female %>% 
  drop_na(age)

ggplot(female1,aes(x = age))+
  geom_histogram(aes(y=..density..),color = "#00A650", fill = "white",alpha = 0.6,binwidth = 2)+
  geom_density(colour = "#FFD790",fill = "#FBB130", alpha = 0.40)+
  labs(x = "Age",
       y = "",
       title = "The Distribution of Female Athlete's Age")+ 
  theme_bw()+
  theme(plot.title = element_text(face="bold", size=16),
        axis.title.x = element_text(size=14),  
        axis.text = element_text(color="#0085C7", size = 12))
```

![](Wang-Project-2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
summary(female1$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.00   20.00   23.00   23.73   27.00   74.00

The results and the histogram showed that the age range of female
athletes was 11-74 years old, but the median age was 23 years and the
mean age was 23.73 years old, indicating that the majority of female
athletes participated in the Olympic Games at a younger age.

## What is the age range for medal-winning female athletes?

``` r
female_medal1 <- female1 %>% 
  drop_na(medal)

ggplot(female_medal1,aes(x = age))+
  geom_histogram(aes(y=..density..),color = "#00A650", fill = "white",alpha = 0.6,binwidth = 2)+
  geom_density(colour = "#FFD790",fill = "#00498E", alpha = 0.40)+
  labs(x = "Age",
       y = "",
       title = "The Distribution of Female Medal-winning Athlete's Age")+ 
  theme_bw()+
  theme(plot.title = element_text(face="bold", size=16),
        axis.title.x = element_text(size=14),  
        axis.text = element_text(color="#0085C7", size = 12))
```

![](Wang-Project-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
summary(female_medal1$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.00   21.00   24.00   24.51   28.00   69.00

The results and the histogram showed that the age range of female
medal-winning athletes was 11-69 years old, but the median age was 24
years and the mean age was 24.51 years old. Compared with the age range
of female athletes, there are around 1 year age differences for the
median and mean age, which indicated that most female athletes win their
Olympic medals before 30 years old.

### The youngest female athletes and the youngest female medal-winning athlete

``` r
age11 <-female1 %>% 
  filter(age == "11")
unique(age11$name)   # youngest female athletes
```

    ## [1] "Magdalena Cecilia Colledge"                     
    ## [2] "Luigina Giavotti"                               
    ## [3] "Sonja Henie (-Topping, -Gardiner, -Onstad)"     
    ## [4] "Beatrice Hutiu"                                 
    ## [5] "Etsuko Inada"                                   
    ## [6] "Liu Luyang"                                     
    ## [7] "Marcelle Matthews"                              
    ## [8] "Megan Olwen Devenish Taylor (-Mandeville-Ellis)"
    ## [9] "Liana Vicens"

``` r
filter(female_medal1, age == "11")   #youngest female medal-winning athlete
```

    ## # A tibble: 1 × 15
    ##      id name      sex     age height weight team  noc   games  year season city 
    ##   <dbl> <chr>     <chr> <dbl>  <dbl>  <dbl> <chr> <chr> <chr> <dbl> <chr>  <chr>
    ## 1 40129 Luigina … F        11     NA     NA Italy ITA   1928…  1928 Summer Amst…
    ## # … with 3 more variables: sport <chr>, event <chr>, medal <chr>

And there were 9 female athletes who participated in the Olympic Games
when they were 11 years old, and most of them were figure skaters who
competed in the Winter Olympics:

**Magdalena Cecilia Colledge (Great Britain, Figure Skating (1932
Winter))** – She was the 1936 Olympic silver medalist, the 1937 World
Champion, the 1937–1939 European Champion, and a six-time (1935–1939,
1946) British national champion. She was also the first female skater to
perform a double jump, as well as being the inventor of both the camel
spin and the layback spin.[^6]

**Luigina Giavotti (Italy, Gymnastics (1928 Summer))** – She was the
youngest medalist of the Amsterdam Games and also the youngest female
Olympic medalist of all time at the age of 11 years. In 1928, she won
the silver medal for the Italian team. [^7]

**Sonja Henie (Norway, Figure Skating (1924 Winter))** – She was a
three-time Olympic champion (1928, 1932, 1936) in women’s singles, a
ten-time World champion (1927–1936) and a six-time European champion
(1931–1936). She also had the most Olympic and World titles in all
ladies’ figure skaters. During her acting career, Haney was one of
Hollywood’s highest-paid stars. [^8] [^9]

**Beatrice Huștiu (Romania, Figure Skating (1968 Winter))** – She
competed at the 1968 Winter Olympics and served as her country’s flag
bearer at the event. [^10]

**Etsuko Inada (Japan, Figure Skating (1936 Winter))** – She was a
seven-time Japanese national champion. In 1952, Inada retired from
competition to become a coach. She not only trained several Olympians
including Miwa Fukuhara, but she also taught figure skating to the
imperial family. [^11] [^12]

**Liu Luyang (China, Figure Skating (1988 Winter))** – She was the 1986
Asian Winter Games champion. [^13]

**Marcelle Matthews (South Africa, Figure Skating (1960 Winter))** – She
was a South African pair skater. With partner Gwyn Jones, she
represented South Africa at the 1960 Winter Olympics where she placed
13th. [^14]

**Megan Olwen Devenish Taylor (Great Britain, Figure Skating (1932
Winter))** – She won the World Championships in 1938 and 1939. With
**Magdalena Cecilia Colledge**, they are the youngest ever female
competitors in any Olympic sport and the youngest ever competitors at
the Winter Olympics. At this year’s winter Olympics, **Sonja Henie** won
her second Olympic gold medal.

**Liana Vicens (Puerto Rico, Swimming (1968 Summer))** – At 11 years
old, she remained the youngest known competitor in the history of the
Olympic Games as of 2016.

### The oldest female athletes and the oldest female medal-winning athlete

``` r
filter(female1, age == "74")  # oldest female athletes
```

    ## # A tibble: 4 × 15
    ##       id name     sex     age height weight team  noc   games  year season city 
    ##    <dbl> <chr>    <chr> <dbl>  <dbl>  <dbl> <chr> <chr> <chr> <dbl> <chr>  <chr>
    ## 1 101272 Ernesti… F        74     NA     NA Fran… FRA   1924…  1924 Summer Paris
    ## 2 101272 Ernesti… F        74     NA     NA Fran… FRA   1924…  1924 Summer Paris
    ## 3 101272 Ernesti… F        74     NA     NA Fran… FRA   1924…  1924 Summer Paris
    ## 4 101272 Ernesti… F        74     NA     NA Fran… FRA   1924…  1924 Summer Paris
    ## # … with 3 more variables: sport <chr>, event <chr>, medal <chr>

``` r
filter(female_medal1, age == "69")   #oldest female medal-winning athlete
```

    ## # A tibble: 1 × 15
    ##      id name      sex     age height weight team  noc   games  year season city 
    ##   <dbl> <chr>     <chr> <dbl>  <dbl>  <dbl> <chr> <chr> <chr> <dbl> <chr>  <chr>
    ## 1 45286 Letitia … F        69     NA     NA Irel… IRL   1948…  1948 Summer Lond…
    ## # … with 3 more variables: sport <chr>, event <chr>, medal <chr>

The oldest female athlete is **Ernestine Lonie Ernesta Robert-Mrignac
(France, Art Competitions (1924))**, who participated in the Art
competitions when she was 74. The oldest female medal-winning athlete is
**Letitia Marion Hamilton (Ireland, Art Competitions (1948))**, who
participated in the Art competitions when she was 69 and won a Bronze
medal for the Irish team. She was an Irish landscape artist.

Art competitions formed part of the modern Olympic Games during its
early years, from 1912 to 1948. Medals were awarded for works of art
inspired by sport, divided into five categories: architecture,
literature, music, painting, and sculpture. [^15]

## What are the most and the least participated sports by females in Summer and Winter Olympic Games?

``` r
female_s <- olympics %>% 
  filter(sex == "F",
         season == "Summer")

female_w <- olympics %>% 
  filter(sex == "F",
         season == "Winter")
```

I noticed that there are many athletes who participated in multiple
events for the same sports category. Therefore, I cleaned the data to
only analyze how many athletes participated in each sports category.

### Summer Olympic Games

``` r
female2<- female_s %>% 
  select(name, sport) %>% 
  group_by(sport) %>%
  distinct(name,.keep_all = TRUE) %>% 
  summarize(participant= n()) %>% 
  arrange(desc(participant))

ggplot(female2,aes(x = reorder(sport,participant),y = participant))+
  geom_col(fill = "#F4C300")+
  coord_flip()+
  labs(x = "Summer Olympic Sport",
       y = "Number of Female Athletes",
       title = "Female Athletes by Summer Olympic Sports")+
  theme(plot.title = element_text(face="bold", size=18),
        axis.title = element_text(size=14),  
        axis.text = element_text(color="#0085C7", size = 10))
```

![](Wang-Project-2_files/figure-gfm/participates%20per%20sport-1.png)<!-- -->

``` r
head(female2)
```

    ## # A tibble: 6 × 2
    ##   sport      participant
    ##   <chr>            <int>
    ## 1 Athletics         6527
    ## 2 Swimming          3618
    ## 3 Gymnastics        1499
    ## 4 Rowing            1483
    ## 5 Volleyball        1128
    ## 6 Handball          1027

``` r
tail(female2)
```

    ## # A tibble: 6 × 2
    ##   sport          participant
    ##   <chr>                <int>
    ## 1 Boxing                  65
    ## 2 Trampolining            44
    ## 3 Figure Skating          18
    ## 4 Croquet                  3
    ## 5 Alpinism                 1
    ## 6 Motorboating             1

There is a total of 40 sports in the Summer Olympic Games. Athletics,
Swimming, and Gymnastics are the top 3 sports with the highest
participation. Motorboating, Alpinism and Croquet had less than 5 female
athletes participating in the whole 120 years of the Olympic history.
Motorboating was an official sport only once at the Olympic Games, in
1908.[^16] Alpinism became an Olympic sport in 1894, was drooped from
the Olympics in 1946. Instead of actual competitions, medals were
awarded for the most notable alpinism feat accomplished during the
previous four years. [^17] Just as Motorboating, the 1900 Summer Olympic
was the only year that Croquet was part of the official sport. Three
women participated in two individual events. [^18]

It is surprising to see Figure skating included in summer Olympic sports
from 1908 to 1920. Figure skating was first contested in the Olympic
Games at the 1908 Summer Olympics. And from 1908 to 1920, figure skating
was contested at Summer Olympic Games. Since 1924, the sport has been a
part of the Winter Olympic Games.[^19]

### Winter Olympic Games

``` r
female3<- female_w %>% 
  select(name, sport) %>% 
  group_by(sport) %>%
  distinct(name,.keep_all = TRUE) %>% 
  summarize(participant= n()) %>% 
  arrange(desc(participant))

ggplot(female3,aes(x = reorder(sport,participant),y = participant))+
  geom_col(fill = "#F4C300")+
  coord_flip()+
  labs(x = "Winter Olympic Sport",
       y = "Number of Female Athletes",
       title = "Female Athletes by Winter Olympic Sports")+
  theme(plot.title = element_text(face="bold", size=18),
        axis.title = element_text(size=14),  
        axis.text = element_text(color="#0085C7", size = 10))
```

![](Wang-Project-2_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
head(female3)
```

    ## # A tibble: 6 × 2
    ##   sport                participant
    ##   <chr>                      <int>
    ## 1 Alpine Skiing                996
    ## 2 Figure Skating               811
    ## 3 Cross Country Skiing         717
    ## 4 Speed Skating                528
    ## 5 Ice Hockey                   498
    ## 6 Biathlon                     371

``` r
tail(female3)
```

    ## # A tibble: 6 × 2
    ##   sport                     participant
    ##   <chr>                           <int>
    ## 1 Luge                              228
    ## 2 Short Track Speed Skating         208
    ## 3 Curling                           160
    ## 4 Bobsleigh                         109
    ## 5 Skeleton                           45
    ## 6 Ski Jumping                        30

There are 14 sports in the Winter Olympic Games. Alpine Skiing, Figure
Skating, and Cross Country Skiing are the top 3 sports with the highest
number of female athletes. Ski Jumping and Skeleton had less than 50
female athletes participating in the whole 120 years of the Olympic
history.

Ski jumping has been a part of the Winter Olympic programme since the
first Games in 1924, however, the women’s competition was only added in
2014.[^20] Skeleton appeared in the Olympic program in 1928 and again in
1948. It was added permanently to the Olympic program for the 2002
Winter Olympics, at which stage a women’s race was added.[^21]

## Variation of Female Medal-winning Athletes over time

Winning an Olympic medal is one of the greatest honors as an athlete. As
more female athletes join the Olympic Games, I would like to analyze
whether there are more female athletes are getting medals over time.

From 1924 to 1992, the Summer and Winter Games were held in the same
year. In addition, there are more sports events in the Summer Olympic
Games than the Winter Olympic Games, which indicates that there will be
more medals awarded in the Summer Olympic Games. In order to have a
better understanding and visualization of the data, I decided to analyze
the total number of medals won by female athletes in the Summer and
Winter Olympic Games separately.

``` r
require(gridExtra)

female_medal_s <- female_s%>% 
  select (year, medal) %>% 
  drop_na(medal) %>% 
  group_by(year) %>% 
  summarize(medal= n())


g1<- ggplot(female_medal_s,aes(x = year,y = medal),filter = "Season")+
  geom_line(color = "#F4C300",lwd = 1.2)+
  geom_point(shape = 11,color = "#009F3D",size =2)+
  xlim(1900, 2020)+
  labs(x = "Year of Olympics",
       y = "Number of Medals",
       title = "Medals winning by Female Athletes in Summer Olympics")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", size=16),
        axis.title = element_text(size=14),  
        axis.text = element_text(size = 12, color = "#0085C7"))

female_medal_w <- female_w%>% 
  select (year, medal) %>% 
  drop_na(medal) %>% 
  group_by(year) %>% 
  summarize(medal= n())


g2 <- ggplot(female_medal_w,aes(x = year,y = medal),filter = "Season")+
  geom_line(color = "#F4C300",lwd = 1.2)+
  geom_point(shape = 11,color = "#009F3D",size =2)+
  xlim(1900, 2020)+
  labs(x = "Year of Olympics",
       y = "Number of Medals",
       title = "Medals winning by Female Athletes in Winter Olympics")+
  theme_bw()+
  theme(plot.title = element_text(face="bold", size=16),
        axis.title = element_text(size=14),  
        axis.text = element_text(size = 12, color = "#0085C7"))

grid.arrange(g1, g2)
```

![](Wang-Project-2_files/figure-gfm/medal%20by%20year-1.png)<!-- -->

It is interesting to see that the 1900 Paris Summer Olympic Games was
the first Olympic Games female athletes competed and won medals. 13
medals(4 Gold, 4 Silver, and 5 Bronze) were awarded to female athletes.

From the graph, it is clear to see that for both the Summer and Winter
Olympics, there is a constant increase in the number of medals won by
female athletes due to the increased participation of female athletes
and the rising number of medal events for female athletes. We can also
see a sharper growing trend in females winning medals at the Winter
Olympics than the Summer Olympics. International Olympic Committee (IOC)
also tries to add more female-only events or mix events to build gender
equality at the Games and to provide more opportunities for female
athletes.[^22]

## Conclusion

We can conclude from the analysis that there are more and more female
athletes participating and winning medals in the Olympic Games. The
history of women at the Olympic Games is obviously mimicking women’s
struggle for equal rights for many centuries. Based on the data from the
International Olympic Committee, the 2020 Tokyo Summer Olympic Games was
the first gender-balanced Games, with female athletes accounting for 48
percent of the total. And Beijing 2022 was the most gender-balanced
Olympic Winter Games to date, with women expected to account for 45
percent of athletes. Omitted in the first modern games in 1896, and
marginalized in the early days of growing Olympics, women slowly but
steadily reach for the piece of well-deserved glory. [^23]

[^1]: <https://olympics.com/en/olympic-games/olympic-results>

[^2]: <https://olympics.com/en/olympic-games/athens-1896>

[^3]: <https://www.history.com/this-day-in-history/first-winter-olympics>

[^4]: <https://www.historians.org/publications-and-directories/perspectives-on-history/summer-2021/the-phantom-olympics-why-japan-forfeited-hosting-the-1940-olympics>

[^5]: <https://ehne.fr/en/encyclopedia/themes/gender-and-europe/gendered-body/women-and-olympic-games>

[^6]: <https://en.wikipedia.org/wiki/Cecilia_Colledge>

[^7]: <https://en.wikipedia.org/wiki/Luigina_Giavotti>

[^8]: <https://en.wikipedia.org/wiki/Sonja_Henie>

[^9]: <https://olympics.com/en/athletes/sonja-henie>

[^10]: <https://en.wikipedia.org/wiki/Beatrice_Hu%C8%99tiu>

[^11]: <https://en.wikipedia.org/wiki/Etsuko_Inada>

[^12]: <https://www.olympedia.org/athletes/81362>

[^13]: <https://en.wikipedia.org/wiki/Liu_Luyang>

[^14]: <https://en.wikipedia.org/wiki/Marcelle_Matthews>

[^15]: <https://en.wikipedia.org/wiki/Art_competitions_at_the_Summer_Olympics>

[^16]: <https://www.topendsports.com/events/discontinued/motorboating.htm#>

[^17]: <https://www.topendsports.com/events/discontinued/alpinism.htm>

[^18]: <https://en.wikipedia.org/wiki/Croquet_at_the_1900_Summer_Olympics>

[^19]: <https://olympics.com/en/news/look-to-the-past-the-curious-debut-of-figure-skating-at-the-olympic-summer-games>

[^20]: <https://olympics.com/en/beijing-2022/sports/ski-jumping/>

[^21]: <https://olympics.com/en/beijing-2022/sports/skeleton/>

[^22]: <https://olympics.com/ioc/news/women-at-the-olympic-winter-games-beijing-2022-all-you-need-to-know>

[^23]: <https://olympics.com/ioc/gender-equality/gender-equality-through-time/at-the-olympic-games>
