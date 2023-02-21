var height = 6; // number of guesses
var width = 5;// length of the word
let result = "NULL"; // the word the user is trying to guess
let guesses = 0; // the amount of guesses the user has made
let streak = JSON.parse(localStorage.getItem('streakscore')); // number of times in a row a user has guessed a word
let score = 0; // users score for that particular word
let gameOver = false; // boolean variable for whether the game is over or not
let letters = [0,0,0,0,0]; // corresponds to each letter of the users guess, 0 = gray, 1 = yellow, 2 = green
let numLetters = [0,0,0,0,0]; // the number of times each letter appears in the word the user is trying to guess, decreases each time the corresponding letter appears in the user's guess
let numLetters2 = [0,0,0,0,0]; // duplicate of the inital number of letters in the word the user is trying to guess (necessary for reset)
const leaderBoard = [[], []]; //Leaderboard array
var Gamelost = false;

var textArr;
var randomNum;
var listLength = 0;


window.onload= function(){
    // Creates the game board
    initialize();
    // Sets result to a random word from the bank
    randWord();
    // Finds the amount of times each letter appears in the result
    numOfLetters(result);
    // Copies the inital amount of times each letter appears to a second array
    for(i = 0; i<5; i++){
        numLetters2[i] = numLetters[i];
    }
}

function initialize(){
    //creating the guessing board 
     listLength = 0;
    for(let r = 0; r < height; r++){
        for(let c =0; c < width; c++){
            let tile = document.createElement("span"); //put tiles beside each other instead of below
            tile.id = r.toString()+"-"+ c.toString();
            tile.classList.add("tile"); // apply our css styling 
            tile.innerText = " ";
            document.getElementById("guessBoard").appendChild(tile)// add in html div "guessBoard"
        }
    }
    gameOver = false;
}

// sets result to a random word from the bank
function randWord () {
    let text = "CLAWS BEANS KNEAD SWIPE KNOCK TABLE ZOOMS PURRS MOUSE YARNS CLIMB LIVES BOOTS LOAFS TABBY MEOWS SLEEP KITTY LASER";
    textArr = text.split(" ");
    randomNum = Math.floor(Math.random() * textArr.length);
    result = textArr[randomNum];
    console.log(result);
}

// calls guess word when the enter key is pressed
document.addEventListener("keyup", function(event) {
    if (event.keyCode === 13) {
        guessWord();
    }
});

function guessWord () {

    // Resets the letters remaining counter for the next guess
    for(i = 0; i<5; i++){
        numLetters[i] = numLetters2[i];
    }

    // Takes guess in from text box
    let guess = document.getElementById('item').value;
    guess = guess.toUpperCase();

    let validGuess = isValid(guess);

    // Checks if a guess is valid, not empty, and that the game is not over
    if(validGuess == true && gameOver == false && guess[4]!=undefined){

        // Displays the letters in the text boxes
        for(i = 0; i<5; i++){
            document.getElementById(guesses+"-"+i).innerText=guess[i];
            letters[i]=0;
        }
        
        // Scans for greens, yellows, and grays in the guess
        scanGreen(guess);
        scanYellow(guess);
        scanGray(guess);

        // Increases the guess counter
        guesses++;

        // Checks if a user has guessed the word and ends the game if so
        if(letters[0] == 2 && letters[1] == 2 && letters[2] == 2 && letters[3] == 2 && letters[4] == 2){
            console.log("You did it! ^:3^");
            gameOver = true;
            Gamelost = false;
            GameEnd(guesses, Gamelost);
        }
        // Checks if a user has ran out of guesses and ends the game if so
        else if(guesses >= 6) {
            console.log("Oh nyoo! The word was: " + result);
            gameOver = true;
            Gamelost = true;
            GameEnd(guesses, Gamelost);
        }

    }
    else if (gameOver == false)
    {
        console.log("Not a valid word");
    }
}

// Finds the number of times each letter appears in the result and sets it in numLetters
function numOfLetters(result){
    for(i = 0; i<5; i++)
    {
        for(j=0; j<5; j++){
            if(result[i]==result[j])
            {
                numLetters[i]++;
            }
        }
    }
    console.log(numLetters);
}

// Scans for green letters in the guess
function scanGreen(guess) {
    for(i=0; i<5; i++)
    {
        if(guess[i]==result[i])
        {
            // Makes correct letters green
            console.log("Letter "+i+" is Green!");
            letters[i] = 2;
            document.getElementById(guesses+"-"+i).classList.add('correct');

            // Duplicate Subtraction
            for(j=0; j<5; j++){
                if(result[i]==result[j]){
                numLetters[i]--;
                }
            }
        }
    }
}

// Scans for yellow letters in the guess
function scanYellow(guess) {
    for(i=0; i<5; i++)
    {
        for(j = 0; j<5; j++){
            if(guess[i]==result[j]&&i!=j&&letters[i]!=2&&numLetters[j]>0)
            {
                console.log("Letter "+i+" is Yellow!");
                letters[i] = 1;
                document.getElementById(guesses+"-"+i).classList.add('yellow');

                // Duplicate Subtraction
                for(k=0; k<5; k++){
                    if(result[j]==result[k]){
                    numLetters[j]--;
                }
            }
                
            }
        }
    }
}

// Scans for gray letters in the guess
function scanGray(guess) {
    //
    for(i=0; i<5; i++) {
        if(letters[i] == 0){
            console.log("Letter "+i+" is Gray!");
            letters[i] = 0;
            document.getElementById(guesses+"-"+i).classList.add('incorrect');
        }

    }
}


//checks if a word is in the dictionary and returns true if it finds it, otherwise it returns false
function isValid(word){

    word = word.toLowerCase();

    dictAB = "aback abase abate abbey abbot abhor abide abled abode abort about above abuse abyss acorn acrid actor acute adage adapt adept admin admit adobe adopt adore adorn adult affix afire afoot afoul after again agape agate agent agile aging aglow agony agora agree ahead aider aisle alarm album alert algae alibi alien align alike alive allay alley allot allow alloy aloft alone along aloof aloud alpha altar alter amass amaze amber amble amend amiss amity among ample amply amuse angel anger angle angry angst anime ankle annex annoy annul anode antic anvil aorta apart aphid aping apnea apple apply apron aptly arbor ardor arena argue arise armor aroma arose array arrow arson artsy ascot ashen aside askew assay asset atoll atone attic audio audit augur aunty avail avert avian avoid await awake award aware awash awful awoke axial axiom axion azure bacon badge badly bagel baggy baker baler balmy banal banjo barge baron basal basic basil basin basis baste batch bathe baton batty bawdy bayou beach beady beans beard beast beech beefy befit began begat beget begin begun being belch belie belle belly below bench beret berry berth beset betel bevel bezel bible bicep biddy bigot bilge billy binge bingo biome birch birth bison bitty black blade blame bland blank blare blast blaze bleak bleat bleed bleep blend bless blimp blind blink bliss blitz bloat block bloke blond blood bloom blown bluer bluff blunt blurb blurt blush board boast bobby boney bongo bonus booby boost booth boots booty booze boozy borax borne bosom bossy botch bough boule bound bowel boxer brace braid brain brake brand brash brass brave bravo brawl brawn bread break breed briar bribe brick bride brief brine bring brink briny brisk broad broil broke brood brook broom broth brown brunt brush brute buddy budge buggy bugle build built bulge bulky bully bunch bunny burly burnt burst bused bushy butch butte buxom buyer bylaw";
    dictCD = "cabal cabby cabin cable cacao cache cacti caddy cadet cagey cairn camel cameo canal candy canny canoe canon caper caput carat cargo carol carry carve caste catch cater catty caulk cause cavil cease cedar cello chafe chaff chain chair chalk champ chant chaos chard charm chart chase chasm cheap cheat check cheek cheer chess chest chick chide chief child chili chill chime china chirp chock choir choke chord chore chose chuck chump chunk churn chute cider cigar cinch circa civic civil clack claim clamp clang clank clash clasp class claws clean clear cleat cleft clerk click cliff climb cling clink cloak clock clone close cloth cloud clout clove clown cluck clued clump clung coach coast cobra cocoa colon color comet comfy comic comma conch condo conic copse coral corer corny couch cough could count coupe court coven cover covet covey cower coyly crack craft cramp crane crank crash crass crate crave crawl craze crazy creak cream credo creed creek creep creme crepe crept cress crest crick cried crier crime crimp crisp croak crock crone crony crook cross croup crowd crown crude cruel crumb crump crush crust crypt cubic cumin curio curly curry curse curve curvy cutie cyber cycle cynic daddy daily dairy daisy dally dance dandy datum daunt dealt death debar debit debug debut decal decay decor decoy decry defer deign deity delay delta delve demon demur denim dense depot depth derby deter detox deuce devil diary dicey digit dilly dimly diner dingo dingy diode dirge dirty disco ditch ditto ditty diver dizzy dodge dodgy dogma doing dolly donor donut dopey doubt dough dowdy dowel downy dowry dozen draft drain drake drama drank drape drawl drawn dread dream dress dried drier drift drill drink drive droit droll drone drool droop dross drove drown druid drunk dryer dryly duchy dully dummy dumpy dunce dusky dusty dutch duvet dwarf dwell dwelt dying";
    dictEF = "eager eagle early earth easel eaten eater ebony eclat edict edify eerie egret eight eject eking elate elbow elder elect elegy elfin elide elite elope elude email embed ember emcee empty enact endow enema enemy enjoy ennui ensue enter entry envoy epoch epoxy equal equip erase erect erode error erupt essay ester ether ethic ethos etude evade event every evict evoke exact exalt excel exert exile exist expel extol extra exult eying fable facet faint fairy faith false fancy fanny farce fatal fatty fault fauna favor feast fecal feign fella felon femme femur fence feral ferry fetal fetch fetid fetus fever fewer fiber fibre ficus field fiend fiery fifth fifty fight filer filet filly filmy filth final finch finer first fishy fixer fizzy fjord flack flail flair flake flaky flame flank flare flash flask fleck fleet flesh flick flier fling flint flirt float flock flood floor flora floss flour flout flown fluff fluid fluke flume flung flunk flush flute flyer foamy focal focus foggy foist folio folly foray force forge forgo forte forth forty forum found foyer frail frame frank fraud freak freed freer fresh friar fried frill frisk fritz frock frond front frost froth frown froze fruit fudge fugue fully fungi funky funny furor furry fussy fuzzy";
    dictGH = "gaffe gaily gamer gamma gamut gassy gaudy gauge gaunt gauze gavel gawky gayer gayly gazer gecko geeky geese genie genre ghost ghoul giant giddy gipsy girly girth given giver glade gland glare glass glaze gleam glean glide glint gloat globe gloom glory gloss glove glyph gnash gnome godly going golem golly gonad goner goody gooey goofy goose gorge gouge gourd grace grade graft grail grain grand grant grape graph grasp grass grate grave gravy graze great greed green greet grief grill grime grimy grind gripe groan groin groom grope gross group grout grove growl grown gruel gruff grunt guard guava guess guest guide guild guile guilt guise gulch gully gumbo gummy guppy gusto gusty gypsy habit hairy halve handy happy hardy harem harpy harry harsh haste hasty hatch hater haunt haute haven havoc hazel heady heard heart heath heave heavy hedge hefty heist helix hello hence heron hilly hinge hippo hippy hitch hoard hobby hoist holly homer honey honor horde horny horse hotel hotly hound house hovel hover howdy human humid humor humph humus hunch hunky hurry husky hussy hutch hydro hyena hymen hyper";
    dictIJKL = "icily icing ideal idiom idiot idler idyll igloo iliac image imbue impel imply inane inbox incur index inept inert infer ingot inlay inlet inner input inter intro ionic irate irony islet issue itchy ivory jaunt jazzy jelly jerky jetty jewel jiffy joint joist joker jolly joust judge juice juicy jumbo jumpy junta junto juror kappa karma kayak kebab khaki kinky kiosk kitty knack knave knead kneed kneel knelt knife knock knoll known koala krill label labor laden ladle lager lance lanky lapel lapse large larva laser lasso latch later lathe latte laugh layer leach leafy leaky leant leapt learn lease leash least leave ledge leech leery lefty legal leggy lemon lemur leper level lever libel liege light liken lilac limbo limit linen liner lingo lipid lithe liver lives livid llama loafs loamy loath lobby local locus lodge lofty logic login loopy loose lorry loser louse lousy lover lower lowly loyal lucid lucky lumen lumpy lunar lunch lunge lupus lurch lurid lusty lying lymph lynch lyric";
    dictMN = "macaw macho macro madam madly mafia magic magma maize major maker mambo mamma mammy manga mange mango mangy mania manic manly manor maple march marry marsh mason masse match matey mauve maxim maybe mayor mealy meant meaty mecca medal media medic melee melon meows mercy merge merit merry metal meter metro micro midge midst might milky mimic mince miner minim minor minty minus mirth miser missy mocha modal model modem mogul moist molar moldy money month moody moose moral moron morph mossy motel motif motor motto moult mound mount mourn mouse mouth mover movie mower mucky mucus muddy mulch mummy munch mural murky mushy music musky musty myrrh nadir naive nanny nasal nasty natal naval navel needy neigh nerdy nerve never newer newly nicer niche niece night ninja ninny ninth noble nobly noise noisy nomad noose north nosey notch novel nudge nurse nutty nylon nymph";
    dictOP = "oaken obese occur ocean octal octet odder oddly offal offer often olden older olive ombre omega onion onset opera opine opium optic orbit order organ other otter ought ounce outdo outer outgo ovary ovate overt ovine ovoid owing owner oxide ozone paddy pagan paint paler palsy panel panic pansy papal paper parer parka parry parse party pasta paste pasty patch patio patsy patty pause payee payer peace peach pearl pecan pedal penal pence penne penny perch peril perky pesky pesto petal petty phase phone phony photo piano picky piece piety piggy pilot pinch piney pinky pinto piper pique pitch pithy pivot pixel pixie pizza place plaid plain plait plane plank plant plate plaza plead pleat plied plier pluck plumb plume plump plunk plush poesy point poise poker polar polka polyp pooch poppy porch poser posit posse pouch pound pouty power prank prawn preen press price prick pride pried prime primo print prior prism privy prize probe prone prong proof prose proud prove prowl proxy prude prune psalm pubic pudgy puffy pulpy pulse punch pupal pupil puppy puree purer purge purrs purse pushy putty pygmy";
    dictQR = "quack quail quake qualm quark quart quash quasi queen queer quell query quest queue quick quiet quill quilt quirk quite quota quote quoth rabbi rabid racer radar radii radio rainy raise rajah rally ralph ramen ranch randy range rapid rarer raspy ratio ratty raven rayon razor reach react ready realm rearm rebar rebel rebus rebut recap recur recut reedy refer refit regal rehab reign relax relay relic remit renal renew repay repel reply rerun reset resin retch retro retry reuse revel revue rhino rhyme rider ridge rifle right rigid rigor rinse ripen riper risen riser risky rival river rivet roach roast robin robot rocky rodeo roger rogue roomy roost rotor rouge rough round rouse route rover rowdy rower royal ruddy ruder rugby ruler rumba rumor rupee rural rusty";
    dictS = "sadly safer saint salad sally salon salsa salty salve salvo sandy saner sappy sassy satin satyr sauce saucy sauna saute savor savoy savvy scald scale scalp scaly scamp scant scare scarf scary scene scent scion scoff scold scone scoop scope score scorn scour scout scowl scram scrap scree screw scrub scrum scuba sedan seedy segue seize semen sense sepia serif serum serve setup seven sever sewer shack shade shady shaft shake shaky shale shall shalt shame shank shape shard share shark sharp shave shawl shear sheen sheep sheer sheet sheik shelf shell shied shift shine shiny shire shirk shirt shoal shock shone shook shoot shore shorn short shout shove shown showy shrew shrub shrug shuck shunt shush shyly siege sieve sight sigma silky silly since sinew singe siren sissy sixth sixty skate skier skiff skill skimp skirt skulk skull skunk slack slain slang slant slash slate slave sleek sleep sleet slept slice slick slide slime slimy sling slink sloop slope slosh sloth slump slung slunk slurp slush slyly smack small smart smash smear smell smelt smile smirk smite smith smock smoke smoky smote snack snail snake snaky snare snarl sneak sneer snide sniff snipe snoop snore snort snout snowy snuck snuff soapy sober soggy solar solid solve sonar sonic sooth sooty sorry sound south sower space spade spank spare spark spasm spawn speak spear speck speed spell spelt spend spent sperm spice spicy spied spiel spike spiky spill spilt spine spiny spire spite splat split spoil spoke spoof spook spool spoon spore sport spout spray spree sprig spunk spurn spurt squad squat squib stack staff stage staid stain stair stake stale stalk stall stamp stand stank stare stark start stash state stave stead steak steal steam steed steel steep steer stein stern stick stiff still stilt sting stink stint stock stoic stoke stole stomp stone stony stood stool stoop store stork storm story stout stove strap straw stray strip strut stuck study stuff stump stung stunk stunt style suave sugar suing suite sulky sully sumac sunny super surer surge surly sushi swami swamp swarm swash swath swear sweat sweep sweet swell swept swift swill swine swing swipe swirl swish swoon swoop sword swore sworn swung synod syrup";
    dictTUV = "tabby table taboo tacit tacky taffy taint taken taker tally talon tamer tango tangy taper tapir tardy tarot taste tasty tatty taunt tawny teach teary tease teddy teeth tempo tenet tenor tense tenth tepee tepid terra terse testy thank theft their theme there these theta thick thief thigh thing think third thong thorn those three threw throb throw thrum thumb thump thyme tiara tibia tidal tiger tight tilde timer timid tipsy titan tithe title toast today toddy token tonal tonga tonic tooth topaz topic torch torso torus total totem touch tough towel tower toxic toxin trace track tract trade trail train trait tramp trash trawl tread treat trend triad trial tribe trice trick tried tripe trite troll troop trope trout trove truce truck truer truly trump trunk truss trust truth tryst tubal tuber tulip tulle tumor tunic turbo tutor twang tweak tweed tweet twice twine twirl twist twixt tying udder ulcer ultra umbra uncle uncut under undid undue unfed unfit unify union unite unity unlit unmet unset untie until unwed unzip upper upset urban urine usage usher using usual usurp utile utter vague valet valid valor value valve vapid vapor vault vaunt vegan venom venue verge verse verso verve vicar video vigil vigor villa vinyl viola viper viral virus visit visor vista vital vivid vixen vocal vodka vogue voice voila vomit voter vouch vowel vying";
    dictWXYZ = "wacky wafer wager wagon waist waive waltz warty waste watch water waver waxen weary weave wedge weedy weigh weird welch welsh wench whack whale wharf wheat wheel whelp where which whiff while whine whiny whirl whisk white whole whoop whose widen wider widow width wield wight willy wimpy wince winch windy wiser wispy witch witty woken woman women woody wooer wooly woozy wordy world worry worse worst worth would wound woven wrack wrath wreak wreck wrest wring wrist write wrong wrote wrung wryly yacht yarns yearn yeast yield young youth zebra zesty zonal zooms"; 

    if (dictAB.indexOf(word) != -1 || dictCD.indexOf(word) != -1 || dictEF.indexOf(word) != -1 || dictGH.indexOf(word) != -1 || dictIJKL.indexOf(word) != -1 || dictMN.indexOf(word) != -1 || dictOP.indexOf(word) != -1 || dictQR.indexOf(word) != -1 || dictS.indexOf(word) != -1 || dictTUV.indexOf(word) != -1 || dictWXYZ.indexOf(word) != -1) {
        word = word.toUpperCase();
        return true;
    } else {
        word = word.toUpperCase();
        return false;
    }

}


//SCORING AND DATA STRUCTS

//Gives score based off of the amount of guesses a user has done
function scoring(tries, Gamelost) {

    //win state
    if (Gamelost == false) {
        console.log("fries");
        streak++;
        score = ((1 - ((tries / 6) / 2)) * 100);
        score = Math.round(score);
        //point system (percentage of one hundred)
        return score;
    }
    else {
        streak = 0;
        return 0;
    }
}

//fills the Y row of the data table (words)
function wordFill() {
    console.log("wordfill");
    for (let i = 0; i < textArr.length; i++) {
        leaderBoard[[0],[i]] = textArr[i];
    }
}

//fills the X row of the data table (score)
function scoreFill() {
    console.log("scorfill");
    let counting = true;
    /*
    //finds limit for the leaderboard
    while (counting) {
        console.log("1");
        if (leaderBoard[[listLength],[randomNum]]) {
            listLength++;
        }
        else {
            counting = false;
        }
    }
    console.log(listLength);
    */
    //Makes sure if the score "exists" already, if it doesn't "exist" it adds the score to the board
    let checkr = true;
    let indx;
    for (let i = 0; i < 6; i++) {
        if (leaderBoard[[i], [randomNum]] == score) {
            indx = i + 1;
            checkr = false;
        }
    } console.log("2");
    if (checkr) {
        leaderBoard[[indx],[randomNum]] = score;

        listLength++;
    } console.log("3");
}

//sorts the scoreboard (one row of this array should look like this by the end WORD: SCORE biggest to SCORE smallest)
function boardSort() {
    console.log("boardSort");
    let temp;

    //uses blubble sort to sort scoreboard, pretty much goes through each element of the scoreboard and compares the current element to the previous one
    //((if curr is smaller than prev the program swaps them)*listLength)*listLength.
    if (listLength > 1) {
        for (let i = 0; i < listLength; i++) {
            for (let j = 1; j < listLength; j++) {
                if (leaderBoard[[j],[randomNum]] > leaderBoard[[j - 1],[randomNum]]) {

                    temp = leaderBoard[[j], [randomNum]];
                    leaderBoard[[j],[randomNum]] = leaderBoard[[j - 1],[randomNum]];
                    leaderBoard[[j - 1],[randomNum]] = temp;
                }
            }
        }
    }
}

//finds the player's position inside the scoreboard
function position() {
    console.log("position");
    //finds the player's leaderboard position
    var PlayrPos;
    for (let i = 0; i < listLength; i++) {
        if (leaderBoard[[i],[randomNum]] == score) {
            PlayrPos = i;
            return PlayrPos;
        }
    }
}

//Game finished
function GameEnd(guesses, Gamelost) {
    console.log("Gameend");
     //guesses is equal to 1 on the first try
    if (!Gamelost) {
        scoring(guesses - 1, Gamelost);
        console.log("position: " + position());
        console.log("listLength: " + listLength);
        console.log("randomNum: " + randomNum);
        console.log("score: " + score);
        console.log("streak: " + streak);
    }
    else {
        //loosing message
        scoring(guesses - 1, Gamelost);
        console.log("position: " + position());
        console.log("listLength: " + listLength);
        console.log("randomNum: " + randomNum);
        console.log("score: " + score);
        console.log("streak: " + streak);
    }
    localStorage.setItem('streakscore', JSON.stringify(streak));
    localStorage.setItem('leaderboard', JSON.stringify(leaderBoard));
}
