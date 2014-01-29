Cleaning the Sandy Twitter data
================================

The purpose of this script is to do basic analysis of the Twitter data. 
It has been found that the input data contains many 'false positives' that 
need to be selectively filtered and removed, as they add noise to the results. 
Once this has been done, basic statistics on the nature of tweets are calculated.
Finally we finish with some visualisations of the data.

## Loading the data


```r
allGeo <- read.csv("allSandyGeo.csv")  # load 65 Mb tweets file
head(allGeo)
```

```
##   actor.followersCount actor.friendsCount actor.languages
## 1                  207                254              en
## 2                  212                214              en
## 3                   25                 47              en
## 4                 1942               1206              en
## 5                  259                250              en
## 6                  792                751              en
##                              actor.link actor.preferredUsername
## 1   http://www.twitter.com/kevinwaldrop            kevinwaldrop
## 2    http://www.twitter.com/NIkKidrauhl             NIkKidrauhl
## 3     http://www.twitter.com/Maruchi319              Maruchi319
## 4  http://www.twitter.com/WendyFournier           WendyFournier
## 5  http://www.twitter.com/Balldawgpetah           Balldawgpetah
## 6 http://www.twitter.com/LenaVanantwerp          LenaVanantwerp
##   actor.statusesCount
## 1                6022
## 2                3003
## 3                  66
## 4                4352
## 5                7650
## 6               10516
##                                                                                                                                           body
## 1 Hurricane Sandy is expected to make landfall in Hobart, IN the night before Thanksgiving whilst swilling wine &amp; rolling out a pie crust.
## 2                                                                                        @GianFadly_SandY hahaha.. Ga jg kok \nKgen indo malah
## 3                                                                                                            This Just In http://t.co/q5Izdkdo
## 4                                             @DeidreLoren I hope they're okay, Dee. Looks like Sandy may be coming to visit us next week too.
## 5                                                                                               @sandybeachcrabs @gynbatits get real gynnnnaaa
## 6                                                                                Im cool with them all but she can throw hands @carolineesandy
##   generator.displayName         lat          lon gnip.klout_score
## 1    Twitter for iPhone 41.93555928 -87.64283545               57
## 2   Twitter for Android  25.0082356  121.4427826               42
## 3   Twitter for Android  40.9225177  -74.1924023                 
## 4    Twitter for iPhone 41.57661068 -71.26556355               64
## 5    Twitter for iPhone 42.10595628 -72.63564307               35
## 6    Twitter for iPhone 39.73206743 -75.06549467               39
##                                                   inReplyTo.link
## 1                                                               
## 2 http://twitter.com/GianFadly_SandY/statuses/261143169981349888
## 3                                                               
## 4     http://twitter.com/DeidreLoren/statuses/261066743433273344
## 5 http://twitter.com/sandybeachcrabs/statuses/261255988018020353
## 6                                                               
##                                                            link
## 1   http://twitter.com/kevinwaldrop/statuses/261255803456090112
## 2    http://twitter.com/NIkKidrauhl/statuses/261255831176232960
## 3     http://twitter.com/Maruchi319/statuses/261256063200935938
## 4  http://twitter.com/WendyFournier/statuses/261256158042533888
## 5  http://twitter.com/Balldawgpetah/statuses/261256165382553601
## 6 http://twitter.com/LenaVanantwerp/statuses/261256591540625410
##   object.actor.displayName object.actor.followersCount
## 1                                                     
## 2                                                     
## 3                                                     
## 4                                                     
## 5                                                     
## 6                                                     
##   object.actor.preferredUsername object.actor.statusesCount
## 1                                                          
## 2                                                          
## 3                                                          
## 4                                                          
## 5                                                          
## 6                                                          
##   object.twitter_entities.hashtags.text               postedTime day hour
## 1                                       2012-10-25T00:00:12.000Z  25    0
## 2                                       2012-10-25T00:00:18.000Z  25    0
## 3                                       2012-10-25T00:01:14.000Z  25    0
## 4                                       2012-10-25T00:01:36.000Z  25    0
## 5                                       2012-10-25T00:01:38.000Z  25    0
## 6                                       2012-10-25T00:03:19.000Z  25    0
##   min sec   comboTime retweetCount twitter_entities.hashtags.text
## 1   0  12 10-25 00:00            0                               
## 2   0  18 10-25 00:00                                            
## 3   1  14 10-25 00:01                                            
## 4   1  36 10-25 00:01                                            
## 5   1  38 10-25 00:01                                            
## 6   3  19 10-25 00:03                                            
##   twitter_entities.user_mentions.screen_name verb
## 1                                            post
## 2                            GianFadly_SandY post
## 3                                            post
## 4                                DeidreLoren post
## 5                  sandybeachcrabs,gynbatits post
## 6                             carolineesandy post
```


## Identifying false positives

It seems that the filter used to select the tweets mentioning hurricane Sandy was 
very crude, harvesting all tweets that contain the character string "sandy" without 
case sensitivity or check that it is the hurricane is the topic. 

We can look at these false positives in a number of ways.

### User names containing Sandy

Users called Sandy are likely to refer to send tweets containing that text string 
to talk about themselves or social events at their house: 


```r
nameSand <- which(grepl("sandy", allGeo$actor.preferredUsername, ignore.case = T))  # all Tweeters with sandy in their name
allGeo$body[nameSand][1:20]  # a look at these tweets shows that they are about people, not the hurricane
```

```
##  [1] #Sandy is trending because of the hurricane.... I'm famous!!!                                                                               
##  [2] Iye juan RT @yuanitagusti: Enjoy aja RT @Yusandysandez: Terlambat hari kamis itu menjadi tertuduh (cont) http://t.co/Rup7tzPF               
##  [3] a dormir (@ casita de sandy) http://t.co/Zw4Kvp3K                                                                                           
##  [4] iyya,,smua ud d'atur,kita tnggal usaha nd ikhtiar ajj,nda bole iri"@Queeneka18: @sandy_anyun rezekinya belom sampe ke anak berarti ya..??"  
##  [5] seneng lah,,lo tnggal nunggu giliran (????)???c) "@Queeneka18: @sandy_anyun sedih euy liat temen gw baru dapet anak perempuan.."            
##  [6] Tau tuh, sok penting banget tuh anak 2-____-RT @AmandaJSarah: @nkainhere @dhe_poohh sandyuuki gak denisa ga siska di borgol smua yeeeee     
##  [7] Tukang makan nih makanya tau (`??)-? RT @AmandaJSarah: @dhe_poohh @nkainhere sandyuuki yg d mm gak enak ! Enakan d gading de                
##  [8] Iyaa di aku bund,kenapose? "@beibhz07: hei @phinesandy poto bunda yg dr nikeu di km apa dmn sayank.?"                                       
##  [9] Tinggal sejam :D (@ Sandy's Home - Pagutan) http://t.co/2HALl9aB                                                                            
## [10] @SandyfordSalon @thechilloutcafe wow just had a great breakfast all set for the day ahead cheers everybody @thechilloutcafe                 
## [11] I'm at Sandy's Home - Pagutan (Mataram, Indonesia) [pic]: http://t.co/dTiqx2Ls                                                              
## [12] "@Maxicollao: Hola hurac?n @sandyboquita ajajj un abrazo."/ayer lei esoy me reimucho. Jaja...vos mejor?besitos!!                            
## [13] puassa kan tyuz sung dance"@Queeneka18: Makanya mandi..RT"@sandy_anyun: njiiirrr laperrrr""                                                 
## [14] baru bed rebahan nii gw"@Queeneka18: @sandy_anyun sini takbiran kerumah gw..ada roti maryam nih.."                                          
## [15] http://t.co/TK5rtFM0 hurricane sandy here to fuck shit up                                                                                   
## [16] Cepet plg ya kakaq (????)? RT@rikhanggraini: Jngn kumpul2 ya qlo gg ada w RT"@arisandy_ecy: (??_??)\\('??'? ) RT@rikhanggraini: Buat sekian2
## [17] Sdh siap kk,kilim aja ya ksna tangkapRT@rikhanggraini: Haha,iya adek,tp buatin soto ya RT"@arisandy_ecy: Cepet plg ya kakaq (????)? RT@rikha
## [18] @smokinjoe_  found me passed out unconscious in my briefs on my bedroom floor early last night. Guess I was wasted. #oldsandyhabits         
## [19] @The_SA_Blog Sandy Ho @ppccedu Director of Learning Assistance Centers #SAchat                                                              
## [20] "@NicolasJannes: @SandyhPeace 16" u live in?                                                                                                
## 190351 Levels: ` Koolingg Outsidee ;) Wt Sandy . ...
```

```r
length(nameSand)/nrow(allGeo)  # these tweets make up only 0.03% of the tweets
```

```
## [1] 0.003125
```


More seriously, tweets
sent to anyone whose name contains Sandy will be selected:

```r
nameLinkSand <- which(grepl("sandy", allGeo$inReplyTo.link, ignore.case = T))  # all tweets to sandys
head(allGeo$body[nameLinkSand])  # again, these mention people, not the hurricane
```

```
## [1] @GianFadly_SandY hahaha.. Ga jg kok \nKgen indo malah                                    
## [2] @sandybeachcrabs @gynbatits get real gynnnnaaa                                           
## [3] @SandyCeree dime qui?n te reclama para golpearlo? Jaja oknoo!! Estuvo bonito el mensajito
## [4] @Sandy2Cheekss @therealjuicyj man I wanna go smh                                         
## [5] @Sandy_Madridsta ??? ?? ? ??????? ?????? ?????? .. ?? ?????\n????? ??? ????              
## [6] @Sandy2Cheekss dawg smh                                                                  
## 190351 Levels: ` Koolingg Outsidee ;) Wt Sandy . ...
```

```r
length(nameLinkSand)/nrow(allGeo)  # 2.3% of tweets affected by this error
```

```
## [1] 0.02342
```


### Sandy not being a complete word

Often the "sandy" string is part of a larger text string that does not
refer to the hurricane ("hurricanesandy" is an exception with 
18313 mentions).
We can identify these false positives using regular expressions:


```r
sSandy <- which(grepl("sandy[[[:alnum:]]", allGeo$body, ignore.case = T))
head(allGeo$body[sSandy], 20)  # note that none of these tweets appear to be directly related to the hurricane
```

```
##  [1] @sandybeachcrabs @gynbatits get real gynnnnaaa                                                                                                  
##  [2] @SandyCeree dime qui?n te reclama para golpearlo? Jaja oknoo!! Estuvo bonito el mensajito                                                       
##  [3] @Sandy2Cheekss @therealjuicyj man I wanna go smh                                                                                                
##  [4] @Sandy2Cheekss dawg smh                                                                                                                         
##  [5] Yes, yes, and yes! ?@Sandy4Obama: @edshow Palin said God wanted her to run, does that mean that HE wanted her to lose then? lol?                
##  [6] ?@sandyquiltz: @SewExcitedQuilt @quiltcabana @quiltscapes A small but very, very special world, LOL.? True dat!                                 
##  [7] Tuning out Sandys rain with techno music hmmm sorry but I don't feel like listening to it                                                       
##  [8] @sandyaolivas  que wapa en su avatar!!                                                                                                          
##  [9] YES SANDYYY BEAR with a solo homer...not that I got to see it! #musicaltheatreproblems #OrangeOctober                                           
## [10] @diansandya hehehe, leres sanget bu. .                                                                                                          
## [11] Wahh cai kan mule buangsat "@Govinda_sandya: Baru hampir , aku mau nya sampe di pecat biar                                                      
## [12] @annisandya no stress no stress, crazy yes but no stress (????)? -___-":p http://t.co/7J2UUPmp                                                  
## [13] Mimpi gendong bayi laki-laki... dlm mimoi itu, Aku seneng... (w/ @sandyafta, Rinaldo, &amp; 6 others at Grand Mega Hotel) ? http://t.co/pJe1Z6TT
## [14] @sandyquiltz this is so far out of my comfort zone - 2 glasses of wine made me do it ;-)                                                        
## [15] @annisandya lah bener kok:| no stress but keep crazy! \\(?????????")/ \\("?????????)/ http://t.co/hC3zkjyo                                      
## [16] @SandyLeah  muito bom!!! Manuscrito ? excelente!!! Certamente vou adorar o novo ?lbum!!! Parab?ns Sandy! A m?sica aquarela dos 30 ? muito...    
## [17] @annisandya ndaamauuu (???) http://t.co/UOvpHg4u                                                                                                
## [18] @SandyLeah ...gostosa de ouvir e uma letra linda!!! Parab?ns!!!                                                                                 
## [19] @nancyjonasrox @michellyxo @victortas @sandywhosoever you mean cowgirls?????                                                                    
## [20] @SandyCeree jaja sigo insistiendo respecto a que suena como guajolote jaja oknoo!!! Te quiero peque?a                                           
## 190351 Levels: ` Koolingg Outsidee ;) Wt Sandy . ...
```

```r
length(sSandy)/nrow(allGeo)  # 13% of tweets contain sandy followed by an alphanumeric character - unlikely to be the storm
```

```
## [1] 0.1365
```

```r
sSandyB <- which(grepl("[abcdfghijklmnopqrstuvwxyx0123456789]sandy", allGeo$body, 
    ignore.case = T))  # sandy tags with alphanumeric prefix excluding e 
head(allGeo$body[sSandyB], 20)  # note that none of these tweets appear to be directly related to the hurricane
```

```
##  [1] wadduw :o tapi uda mulai jual tiket yg onthespot blm dan? "@DannyIndraSandy: Tiket pre-sale sold out, hail @SmadagaskarSolo hail \\m/"
##  [2] @diansandya hehehe, leres sanget bu. .                                                                                                
##  [3] @annisandya no stress no stress, crazy yes but no stress (????)? -___-":p http://t.co/7J2UUPmp                                        
##  [4] @MonnyRuizG agarrense qu la.otra semana como.viernes llego yupi!! @DaktariSandy tenemos que irnos a Monterrey!!                       
##  [5] @annisandya lah bener kok:| no stress but keep crazy! \\(?????????")/ \\("?????????)/ http://t.co/hC3zkjyo                            
##  [6] #SomeoneIMiss @MikailaSandy )':                                                                                                       
##  [7] @annisandya ndaamauuu (???) http://t.co/UOvpHg4u                                                                                      
##  [8] #Tweetapicthatdecribesyourfriendship @Tysandyalice http://t.co/QWmyvwsu                                                               
##  [9] "@SandraItzel: @intensas_sandra @Sandra_ItzelSp @FanClubSandy mis amores! los amo mucho!"/y nosotros a ti princesa ???                
## [10] Cieeeeh RT @abearisandy: Buruan ge lu balik,,,gda yang jenggutin rambut gue nii.. http://t.co/FCUGwhul                                
## [11] @zachrisandy laban nadai org tlg dani ke nuan? Haha                                                                                   
## [12] Maacih. ;))RT @abearisandy: SEMEA ( Semangat Eaaaa ) wkwkwkwk RT @yadisti: Serius dis serius!!!!!                                     
## [13] @KnkleHeadSandy lol getting the fit together now! ??                                                                                  
## [14] @_92SaNdY grax m?ster                                                                                                                 
## [15] @KnkleHeadSandy Revolution.                                                                                                           
## [16] Ah nyamber ???  kaya gledek :P RT @RizhaMeisandy: Masih banyak sungai mengalir..RT @winny_pohan: Setujuuuuu :) RT @ay_35ha            
## [17] @1349Sandy que maldad:o                                                                                                               
## [18] @DaktariSandy @MonnyRuizG fin del 3 de nov pero muero x ir a Moknterrey aunque no compre vip pero amos no? Hay q planearlo            
## [19] @KnkleHeadSandy get this sweat on lol                                                                                                 
## [20] @mynamessandy dude I can come get ya Tom at like 7:30?                                                                                
## 190351 Levels: ` Koolingg Outsidee ;) Wt Sandy . ...
```

```r
length(sSandyB)/nrow(allGeo)  # 5% of tweets contain sandy followed by an alphanumeric character - unlikely to be the storm
```

```
## [1] 0.0543
```


### Excluding the false positives

It is suggested we exclude all the likely false positives identified in the previous steps:


```r
asGeo <- allGeo[-sSandy, ]
asGeo <- asGeo[-sSandyB, ]
asGeo <- asGeo[-nameLinkSand, ]
asGeo <- asGeo[-nameSand, ]

1 - nrow(asGeo)/nrow(allGeo)
```

```
## [1] 0.2015
```


The above code shows that 20% of the false positive tweets have now been removed.




