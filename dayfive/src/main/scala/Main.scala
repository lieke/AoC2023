import DayFive.findTheDestination

import scala.collection.mutable.ListBuffer

@main def DayFive2023: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day four of the Advent of Code 2023!")
  val lowestLocationNumber = List(findTheDestination(BigInt("919339981")), findTheDestination(BigInt("562444630")), findTheDestination(BigInt("3366006921")), findTheDestination(BigInt("67827214")), findTheDestination(BigInt("1496677366")), findTheDestination(BigInt("101156779")), findTheDestination(BigInt("4140591657")), findTheDestination(BigInt("5858311")), findTheDestination(BigInt("2566406753")), findTheDestination(BigInt("71724353")), findTheDestination(BigInt("2721360939")), findTheDestination(BigInt("35899538")), findTheDestination(BigInt("383860877")), findTheDestination(BigInt("424668759")), findTheDestination(BigInt("3649554897")), findTheDestination(BigInt("442182562")), findTheDestination(BigInt("2846055542")), findTheDestination(BigInt("49953829")), findTheDestination(BigInt("2988140126")), findTheDestination(BigInt("256306471"))).sortWith(_ < _).head
  println("The lowest location number is " + lowestLocationNumber)
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")

}

object DayFive {

  def findTheDestination(seed: BigInt): BigInt = {
    var destinationLocation = seed
    for (conversion <- conversionMap) {
      destinationLocation = mapToNewValue(conversion, destinationLocation)
    }
    destinationLocation
  }

  def mapToNewValue(newMap: List[(BigInt, BigInt, BigInt)], value: BigInt): BigInt = {
    val possibleDestinations = newMap.map(range => mapToPossibleNewValue(range, value)).distinct
    var destination: BigInt = 0
    if (possibleDestinations.length == 1) {
      destination = value
    } else {
      destination = possibleDestinations.filter(_ != value).head
    }
    destination
  }

  def mapToPossibleNewValue(rangeConversion: (BigInt, BigInt, BigInt), value: BigInt): BigInt = {
    val (destinationStart, sourceStart, rangeLength) = rangeConversion
    if (value >= sourceStart && value < sourceStart + rangeLength) {
      destinationStart + (value - sourceStart)
    } else {
      value
    }
  }

  val conversionMap: List[List[(BigInt, BigInt, BigInt)]] = List(List((BigInt("627617777"), BigInt("1691901751"), BigInt("235673208")),
    (BigInt("2425244517"), BigInt("2483951770"), BigInt("157286279")),
    (BigInt("1339042890"), BigInt("1549225044"), BigInt("142676707")),
    (BigInt("481294110"), BigInt("381503165"), BigInt("89539853")),
    (BigInt("863290985"), BigInt("1007717708"), BigInt("39103521")),
    (BigInt("570833963"), BigInt("324719351"), BigInt("56783814")),
    (BigInt("3953140805"), BigInt("3714151881"), BigInt("155523737")),
    (BigInt("902394506"), BigInt("1941176275"), BigInt("61481385")),
    (BigInt("963875891"), BigInt("675869083"), BigInt("331848625")),
    (BigInt("1922840702"), BigInt("1046821229"), BigInt("502403815")),
    (BigInt("1481719597"), BigInt("1927574959"), BigInt("13601316")),
    (BigInt("1820040264"), BigInt("573068645"), BigInt("102800438")),
    (BigInt("1295724516"), BigInt("2641238049"), BigInt("43318374")),
    (BigInt("0"), BigInt("2002657660"), BigInt("481294110")),
    (BigInt("1495320913"), BigInt("0"), BigInt("324719351")),
    (BigInt("2582530796"), BigInt("471043018"), BigInt("102025627")),
    (BigInt("3714151881"), BigInt("3869675618"), BigInt("238988924"))),

    List((BigInt("1288462652"), BigInt("3191328122"), BigInt("309853381")),
      (BigInt("3216116191"), BigInt("1774097401"), BigInt("151922673")),
      (BigInt("1739360920"), BigInt("2276789875"), BigInt("44162492")),
      (BigInt("220941763"), BigInt("1080382821"), BigInt("325206789")),
      (BigInt("2416141229"), BigInt("3949354107"), BigInt("345613189")),
      (BigInt("1783523412"), BigInt("765967805"), BigInt("117439234")),
      (BigInt("2876214366"), BigInt("1010215130"), BigInt("70167691")),
      (BigInt("2761754418"), BigInt("3142514478"), BigInt("48813644")),
      (BigInt("3840090062"), BigInt("3541735844"), BigInt("44042641")),
      (BigInt("2946382057"), BigInt("1417076798"), BigInt("106871618")),
      (BigInt("3750331405"), BigInt("3501181503"), BigInt("29948437")),
      (BigInt("1105413398"), BigInt("1405589610"), BigInt("11487188")),
      (BigInt("2867709608"), BigInt("3585778485"), BigInt("8504758")),
      (BigInt("3053253675"), BigInt("3594283243"), BigInt("162862516")),
      (BigInt("15940804"), BigInt("0"), BigInt("90040993")),
      (BigInt("2192246779"), BigInt("537548320"), BigInt("223894450")),
      (BigInt("210335859"), BigInt("3531129940"), BigInt("10605904")),
      (BigInt("546148552"), BigInt("2351877346"), BigInt("559264846")),
      (BigInt("1598316033"), BigInt("883407039"), BigInt("110119908")),
      (BigInt("2851021425"), BigInt("993526947"), BigInt("16688183")),
      (BigInt("0"), BigInt("90040993"), BigInt("15940804")),
      (BigInt("4134281688"), BigInt("3757145759"), BigInt("160685608")),
      (BigInt("3718808665"), BigInt("3917831367"), BigInt("31522740")),
      (BigInt("1116900586"), BigInt("2970952412"), BigInt("171562066")),
      (BigInt("2810568062"), BigInt("210335859"), BigInt("40453363")),
      (BigInt("3780279842"), BigInt("2911142192"), BigInt("59810220")),
      (BigInt("1900962646"), BigInt("761442770"), BigInt("4525035")),
      (BigInt("1905487681"), BigInt("250789222"), BigInt("286759098")),
      (BigInt("3368038864"), BigInt("1926020074"), BigInt("350769801")),
      (BigInt("1708435941"), BigInt("2320952367"), BigInt("30924979")),
      (BigInt("3884132703"), BigInt("1523948416"), BigInt("250148985"))),

    List((BigInt("2450598719"), BigInt("3993777626"), BigInt("178688420")),
      (BigInt("990387307"), BigInt("2284751995"), BigInt("36035279")),
      (BigInt("1479519873"), BigInt("1430606124"), BigInt("25388869")),
      (BigInt("2928263979"), BigInt("2233553333"), BigInt("51198662")),
      (BigInt("1825810145"), BigInt("1093816339"), BigInt("30855096")),
      (BigInt("3131362216"), BigInt("1499369679"), BigInt("38622365")),
      (BigInt("2629287139"), BigInt("1980229176"), BigInt("243592551")),
      (BigInt("2979462641"), BigInt("1827354243"), BigInt("146328983")),
      (BigInt("937194740"), BigInt("3188384434"), BigInt("9817881")),
      (BigInt("2069360741"), BigInt("1973683226"), BigInt("975358")),
      (BigInt("1856665241"), BigInt("3781082126"), BigInt("212695500")),
      (BigInt("642830371"), BigInt("945238436"), BigInt("148577903")),
      (BigInt("4114154511"), BigInt("1124671435"), BigInt("95554104")),
      (BigInt("3777484412"), BigInt("483834992"), BigInt("305990165")),
      (BigInt("3264059981"), BigInt("2427531551"), BigInt("296117669")),
      (BigInt("272170557"), BigInt("2723649220"), BigInt("370659814")),
      (BigInt("262438951"), BigInt("2223821727"), BigInt("9731606")),
      (BigInt("1615429560"), BigInt("1220225539"), BigInt("210380585")),
      (BigInt("1028349507"), BigInt("226445100"), BigInt("83554339")),
      (BigInt("4209708615"), BigInt("3198202315"), BigInt("85258681")),
      (BigInt("3169984581"), BigInt("3094309034"), BigInt("94075400")),
      (BigInt("4083474577"), BigInt("309999439"), BigInt("30679934")),
      (BigInt("2872879690"), BigInt("115495593"), BigInt("55384289")),
      (BigInt("2138362191"), BigInt("340679373"), BigInt("4164957")),
      (BigInt("947012621"), BigInt("1455994993"), BigInt("43374686")),
      (BigInt("791408274"), BigInt("1606018136"), BigInt("145786466")),
      (BigInt("1026422586"), BigInt("1825427322"), BigInt("1926921")),
      (BigInt("1578531462"), BigInt("789825157"), BigInt("8469921")),
      (BigInt("3670740135"), BigInt("2320787274"), BigInt("106744277")),
      (BigInt("2272532251"), BigInt("170879882"), BigInt("55565218")),
      (BigInt("3125791624"), BigInt("1974658584"), BigInt("5570592")),
      (BigInt("1111903846"), BigInt("3283460996"), BigInt("367616027")),
      (BigInt("2070336099"), BigInt("1537992044"), BigInt("68026092")),
      (BigInt("1504908742"), BigInt("1751804602"), BigInt("73622720")),
      (BigInt("2142527148"), BigInt("3651077023"), BigInt("130005103")),
      (BigInt("2328097469"), BigInt("4172466046"), BigInt("122501250")),
      (BigInt("1587001383"), BigInt("344844330"), BigInt("28428177")),
      (BigInt("3560177650"), BigInt("373272507"), BigInt("110562485")),
      (BigInt("115495593"), BigInt("798295078"), BigInt("146943358"))),

    List((BigInt("220561404"), BigInt("643114856"), BigInt("123713527")),
      (BigInt("1087373312"), BigInt("4123526389"), BigInt("171440907")),
      (BigInt("0"), BigInt("766828383"), BigInt("220561404")),
      (BigInt("2907464644"), BigInt("3955968868"), BigInt("167557521")),
      (BigInt("376259478"), BigInt("31984547"), BigInt("611130309")),
      (BigInt("344274931"), BigInt("0"), BigInt("31984547")),
      (BigInt("4209967986"), BigInt("1266450159"), BigInt("84999310")),
      (BigInt("3234177104"), BigInt("1246528251"), BigInt("19921908")),
      (BigInt("4075482989"), BigInt("2729348487"), BigInt("134484997")),
      (BigInt("1258814219"), BigInt("1870560966"), BigInt("858787521")),
      (BigInt("3075022165"), BigInt("1087373312"), BigInt("159154939")),
      (BigInt("2439602258"), BigInt("3906331676"), BigInt("49637192")),
      (BigInt("2297013316"), BigInt("3685217461"), BigInt("142588942")),
      (BigInt("2567764723"), BigInt("1530861045"), BigInt("339699921")),
      (BigInt("3254099012"), BigInt("2863833484"), BigInt("821383977")),
      (BigInt("2489239450"), BigInt("3827806403"), BigInt("78525273")),
      (BigInt("2117601740"), BigInt("1351449469"), BigInt("179411576"))),

    List((BigInt("83647742"), BigInt("560398800"), BigInt("311449275")),
      (BigInt("4201716419"), BigInt("3337900402"), BigInt("93250877")),
      (BigInt("1071565427"), BigInt("3024001249"), BigInt("4392221")),
      (BigInt("3424420254"), BigInt("3520028211"), BigInt("161472091")),
      (BigInt("1227070419"), BigInt("2828368402"), BigInt("116428819")),
      (BigInt("0"), BigInt("2744720660"), BigInt("83647742")),
      (BigInt("4199359339"), BigInt("3431151279"), BigInt("2357080")),
      (BigInt("1807547949"), BigInt("2359072262"), BigInt("384093366")),
      (BigInt("3336003529"), BigInt("3486377667"), BigInt("33650544")),
      (BigInt("1075957648"), BigInt("871848075"), BigInt("149557739")),
      (BigInt("3585892345"), BigInt("3681500302"), BigInt("613466994")),
      (BigInt("955495817"), BigInt("2243002652"), BigInt("116069610")),
      (BigInt("2191641315"), BigInt("1406250497"), BigInt("836752155")),
      (BigInt("3369654073"), BigInt("3433508359"), BigInt("52869308")),
      (BigInt("1343499238"), BigInt("2944797221"), BigInt("79204028")),
      (BigInt("3422523381"), BigInt("3336003529"), BigInt("1896873")),
      (BigInt("395097017"), BigInt("0"), BigInt("560398800")),
      (BigInt("1225515387"), BigInt("2743165628"), BigInt("1555032")),
      (BigInt("1422703266"), BigInt("1021405814"), BigInt("384844683"))),

    List((BigInt("3340701300"), BigInt("3627322367"), BigInt("174281926")),
      (BigInt("3514983226"), BigInt("4012096602"), BigInt("282870694")),
      (BigInt("403969772"), BigInt("2344934648"), BigInt("125307793")),
      (BigInt("356249525"), BigInt("1299750763"), BigInt("47720247")),
      (BigInt("529277565"), BigInt("0"), BigInt("19150241")),
      (BigInt("2425473363"), BigInt("606907612"), BigInt("15105367")),
      (BigInt("1622667408"), BigInt("1693081570"), BigInt("188915118")),
      (BigInt("1858084350"), BigInt("1347471010"), BigInt("47053630")),
      (BigInt("4223873328"), BigInt("2752952198"), BigInt("71093968")),
      (BigInt("2749048741"), BigInt("3035669808"), BigInt("188955792")),
      (BigInt("0"), BigInt("2134178305"), BigInt("190659232")),
      (BigInt("1112200615"), BigInt("96440819"), BigInt("510466793")),
      (BigInt("4148180263"), BigInt("2749048741"), BigInt("3903457")),
      (BigInt("1811582526"), BigInt("1113827741"), BigInt("11131074")),
      (BigInt("548427806"), BigInt("2324837537"), BigInt("20097111")),
      (BigInt("1905137980"), BigInt("2051821050"), BigInt("82357255")),
      (BigInt("3116507691"), BigInt("3224625600"), BigInt("224193609")),
      (BigInt("1822713600"), BigInt("1657710820"), BigInt("35370750")),
      (BigInt("568524917"), BigInt("622012979"), BigInt("491814762")),
      (BigInt("2440578730"), BigInt("66777108"), BigInt("29663711")),
      (BigInt("2250681415"), BigInt("1124958815"), BigInt("174791948")),
      (BigInt("4009477562"), BigInt("3873393901"), BigInt("138702701")),
      (BigInt("190659232"), BigInt("1881996688"), BigInt("165590293")),
      (BigInt("1987495235"), BigInt("1394524640"), BigInt("263186180")),
      (BigInt("1107966546"), BigInt("2047586981"), BigInt("4234069")),
      (BigInt("1060339679"), BigInt("19150241"), BigInt("47626867")),
      (BigInt("4210822392"), BigInt("3801604293"), BigInt("13050936")),
      (BigInt("2938004533"), BigInt("3448819209"), BigInt("178503158")),
      (BigInt("3797853920"), BigInt("2824046166"), BigInt("211623642")),
      (BigInt("4152083720"), BigInt("3814655229"), BigInt("58738672"))),

    List((BigInt("2102802203"), BigInt("2756269781"), BigInt("87468599")),
      (BigInt("2877112183"), BigInt("2931341663"), BigInt("22549647")),
      (BigInt("3494879649"), BigInt("1477788233"), BigInt("182041959")),
      (BigInt("2436026725"), BigInt("2953891310"), BigInt("81993792")),
      (BigInt("2899661830"), BigInt("373999642"), BigInt("244663414")),
      (BigInt("2190270802"), BigInt("850802545"), BigInt("169763790")),
      (BigInt("3866824018"), BigInt("90439080"), BigInt("13573105")),
      (BigInt("320139638"), BigInt("1659830192"), BigInt("113214403")),
      (BigInt("1131501111"), BigInt("1773044595"), BigInt("246249128")),
      (BigInt("2417984929"), BigInt("828638896"), BigInt("16505804")),
      (BigInt("3839766923"), BigInt("845144700"), BigInt("5657845")),
      (BigInt("4161134012"), BigInt("2843738380"), BigInt("87603283")),
      (BigInt("3731370785"), BigInt("3801964390"), BigInt("108396138")),
      (BigInt("1003659815"), BigInt("3442085055"), BigInt("127841296")), (BigInt("918426975"), BigInt("3986208955"), BigInt("85232840")), (BigInt("433354041"), BigInt("618663056"), BigInt("209975840")), (BigInt("2811371438"), BigInt("0"), BigInt("65740745")), (BigInt("2518020517"), BigInt("1329719948"), BigInt("148068285")), (BigInt("651651860"), BigInt("3035885102"), BigInt("266775115")), (BigInt("0"), BigInt("2256642811"), BigInt("320139638")), (BigInt("2666088802"), BigInt("228717006"), BigInt("145282636")), (BigInt("4136435677"), BigInt("65740745"), BigInt("24698335")), (BigInt("3144325244"), BigInt("1046598285"), BigInt("188874270")), (BigInt("1657153777"), BigInt("2576782449"), BigInt("179487332")), (BigInt("1844732214"), BigInt("3569926351"), BigInt("232038039")), (BigInt("643329881"), BigInt("2077244060"), BigInt("8321979")), (BigInt("1471997632"), BigInt("4071441795"), BigInt("175759508")), (BigInt("2076770253"), BigInt("1020566335"), BigInt("26031950")), (BigInt("3676921608"), BigInt("3931759778"), BigInt("54449177")), (BigInt("3997010839"), BigInt("3302660217"), BigInt("139424838")), (BigInt("3845424768"), BigInt("3910360528"), BigInt("21399250")), (BigInt("1836641109"), BigInt("104012185"), BigInt("8091105")), (BigInt("3333199514"), BigInt("2085566039"), BigInt("161680135")), (BigInt("3880397123"), BigInt("112103290"), BigInt("116613716")), (BigInt("2360034592"), BigInt("2019293723"), BigInt("57950337")), (BigInt("1377750239"), BigInt("1235472555"), BigInt("94247393")), (BigInt("1647757140"), BigInt("2247246174"), BigInt("9396637")), (BigInt("2434490733"), BigInt("4247201303"), BigInt("1535992"))))

}


