module QuizData exposing (
  jqxz
  , only_cons
  , cons_and_y
  , just_vowels
  , hasq
  , q_no_u
  , threes
  , twos
  )

wordlist : QuizList -> List String
wordlist q =
    case q of
        Twos -> twos
        Threes -> threes 
        Cons -> only_cons
        Qnou -> q_no_u
        Q -> hasq
        Jqxz -> jqxz
        ConsY -> cons_and_y
        JustVowels -> just_vowels

jqxz = ["AZULEJO","BANJAX","BEZIQUE","BIJOUX","CAZIQUE"
        ,"EQUINOX","EXEQUY","JEEZ","JEEZE","JEEZELY","JEEZLY"
        ,"JEUX","JEZAIL","JEZAILS","JEZEBEL","JINX","JINXED"
        ,"JINXES","JINXING","JONQUIL","JUKEBOX","MEZQUIT"
        ,"MUZJIK","MUZJIKS","OUTJINX","OXAZINE","OXAZOLE"
        ,"OXIDIZE","QUARTZ","QUEAZY","QUETZAL","QUEZAL"
        ,"QUEZALS","QUINZIE","QUIXOTE","QUIZ","SQUEEZE"
        ,"ZAX","ZAXES"]

only_cons= ["BRR","BRRR","CRWTH","CRWTHS","CWM","CWMS"
        ,"GRR","GRRRL","GRRRLS","HM","HMM","MM","MMM"
        ,"NTH","PFFT","PHPHT","PHT","PSST","PST","SH"
        ,"SHH","SHHH","TSK","TSKS","TSKTSK","TSKTSKS","ZZZ"]

cons_and_y = ["BY","BYRL","BYRLS","BYS","CRY","CRYPT"
        ,"CRYPTS","CYST","CYSTS","DRY","DRYLY","DRYS"
        ,"FLY","FLYBY","FLYBYS","FLYSCH","FRY","GHYLL"
        ,"GHYLLS","GLYCYL","GLYCYLS","GLYPH","GLYPHS"
        ,"GYM","GYMS","GYP","GYPS","GYPSY","HWYL","HWYLS"
        ,"HYMN","HYMNS","HYP","HYPS","LYCH","LYMPH"
        ,"LYMPHS","LYNCH","LYNX","MY","MYC","MYCS","MYRRH"
        ,"MYRRHS","MYRRHY","MYTH","MYTHS","MYTHY","NYMPH"
        ,"NYMPHS","PLY","PRY","PSYCH","PSYCHS","PYGMY"
        ,"PYX","RHYTHM","RHYTHMS","RYND","RYNDS","SCRY"
        ,"SHY","SHYLY","SKRY","SKY","SLY","SLYLY","SPHYNX"
        ,"SPRY","SPRYLY","SPY","STY","STYMY","SYLPH"
        ,"SYLPHS","SYLPHY","SYN","SYNC","SYNCH","SYNCHS"
        ,"SYNCS","SYNTH","SYNTHS","SYPH","SYPHS","SYZYGY"
        ,"THY","THYMY","TRY","TRYST","TRYSTS","TYPP","TYPPS"
        ,"TYPY","WHY","WHYS","WRY","WRYLY","WYCH","WYN","WYND"
        ,"WYNDS","WYNN","WYNNS","WYNS","XYLYL","XYLYLS","XYST","XYSTS"]

just_vowels = ["AA","AE","AI","EAU","OE","OI"]

hasq = ["BUQSHA","BUQSHAS","BURQA","BURQAS","CINQ","CINQS"
        ,"FAQIR","FAQIRS","KAMOTIQ","KAMOTIQS","MBAQANGA"
        ,"MBAQANGAS","NIQAAB","NIQAABS","NIQAB","NIQABS","QABALA","QABALAH","QABALAHS","QABALAS","QADI","QADIS","QAID","QAIDS","QAJAQ","QAJAQS","QAMUTIK","QAMUTIKS","QANAT","QANATS","QAT","QATS","QAWWALI","QAWWALIS","QI","QIBLA","QIBLAS","QIGONG","QIGONGS","QINDAR","QINDARKA","QINDARS","QINTAR","QINTARS","QIS","QIVIUT","QIVIUTS","QOPH","QOPHS","QWERTY","QWERTYS","SHEQALIM","SHEQEL","SHEQELS","SUQ","SUQS","TRANQ","TRANQS","UMIAQ","UMIAQS"]

q_no_u = ["CINQ","CINQS","FAQIR","FAQIRS","KAMOTIQ","KAMOTIQS","MBAQANGA","MBAQANGAS","NIQAAB","NIQAABS","NIQAB","NIQABS","QABALA","QABALAH","QABALAHS","QABALAS","QADI","QADIS","QAID","QAIDS","QAJAQ","QAJAQS","QANAT","QANATS","QAT","QATS","QAWWALI","QAWWALIS","QI","QIBLA","QIBLAS","QIGONG","QIGONGS","QINDAR","QINDARKA","QINDARS","QINTAR","QINTARS","QIS","QOPH","QOPHS","QWERTY","QWERTYS","SHEQALIM","SHEQEL","SHEQELS","TRANQ","TRANQS"]

threes =  []

twos = ["AA","AB","AD","AE","AG","AH","AI","AL","AM","AN","AR","AS","AT","AW","AX","AY","BA","BE","BI","BO","BY","DA","DE","DO","ED","EF","EH","EL","EM","EN","ER","ES","ET","EX","FA","FE","GI","GO","HA","HE","HI","HM","HO","ID","IF","IN","IS","IT","JO","KA","KI","LA","LI","LO","MA","ME","MI","MM","MO","MU","MY","NA","NE","NO","NU","OD","OE","OF","OH","OI","OM","ON","OP","OR","OS","OW","OX","OY","PA","PE","PI","PO","QI","RE","SH","SI","SO","TA","TE","TI","TO","UH","UM","UN","UP","US","UT","WE","WO","XI","XU","YA","YE","YO","ZA"]
