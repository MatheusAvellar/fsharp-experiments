open System

module still_alive =
    Console.Clear()
    Console.WindowWidth <- 70
    Console.BufferWidth <- 70
    Console.WindowHeight <- 25
    Console.BufferHeight <- 25
    Console.ForegroundColor <- ConsoleColor.Yellow
    Console.Title <- "Still Alive"
    printf " "

    let notes =  // Note frequencies according to http://www.phy.mtu.edu/~suits/notefreqs.html
        dict[
            "A3",  220;
            "Bb3", 233;
            "C4",  261;
            "C#4", 277;
            "D4",  293;
            "E4",  330;
            "F4",  349;
            "F#4", 370;
            "G4",  392;
            "A4",  440;
            "Bb4", 466;
            "B4",  493;
        ]

    let beep(note:string, time:int, lyric:string) = printf "%s" lyric; Console.Beep(notes.Item(note), time);
    let sleep(time:int) = Threading.Thread.Sleep(time);
    let clear() = Console.Clear(); printf " ";

    [<EntryPoint>]
    let main argv = 
        let time = 1999  // 2000 is too much, 1999 falls a bit short ¯\_(ツ)_/¯

        // The big old Beep'n'Sleep
        sleep(1000)
        beep("G4", time/8, "This ")
        beep("F#4", time/8, "was ")
        beep("E4", time/8, "a ")
        beep("E4", time/8, "tri")
        beep("F#4", time/8, "umph.\n ")
        sleep(time/8 + time/4 + time/2 + time/4 + time/8)
        beep("A3", time/8, "I'm ")
        beep("G4", time/8, "ma")
        beep("F#4", time/8, "king ")
        beep("E4", time/8, "a ")
        beep("E4", time/8 + time/8, "note ")
        beep("F#4", time/8, "here:\n ")
        sleep(time/4)
        beep("D4", time/4, "HUGE ")
        beep("E4", time/8, "SUC")
        beep("A3", time/8 + time/8, "CESS.\n ")
        sleep(time/4 + time/4 + time/8 + time/8)
        beep("A3", time/8, "It's ")
        beep("E4", time/4, "hard ")
        beep("F#4", time/8, "to ")
        beep("G4", time/8 + time/4, "o")
        beep("E4", time/8, "ver")
        beep("C#4", time/8 + time/8, "state\n ")
        beep("D4", time/4 + time/8, "my ")
        beep("E4", time/4, "sa")
        beep("A3", time/8, "tis")
        beep("A3", time/8 + time/8, "fac")
        beep("F#4", time/4, "tion.\n ")
        sleep(time/8 + time/2 + time/2)
        beep("G4", time/8, "A")
        beep("F#4", time/8, "per")
        beep("E4", time/8, "ture ")
        beep("E4", time/8, "Sci")
        beep("F#4", time/8, "ence\n ")
        sleep(time/2 + time/4 + time/4 + time/8 + time/8)
        beep("A3", time/8, "We ")
        beep("G4", time/8, "do ")
        beep("F#4", time/8, "what ")
        beep("E4", time/8, "we ")
        beep("E4", time/8, "must\n ")
        sleep(time/4)
        beep("F#4", time/8, "be")
        beep("D4", time/8, "cause ")
        sleep(time/4)
        beep("E4", time/8, "we ")
        beep("A3", time/8 + time/8, "can.\n ")
        sleep(time/4 + time/8 + time/2)
        beep("E4", time/4, "For ")
        beep("F#4", time/8, "the ")
        beep("G4", time/8 + time/4, "good ")
        beep("E4", time/8, "of ")
        beep("C#4", time/8 + time/4, "all ")
        beep("D4", time/8, "of ")
        beep("E4", time/8, "us.\n ")
        sleep(time/8)
        beep("A3", time/8, "Ex")
        beep("D4", time/8, "cept ")
        beep("E4", time/8, "the ")
        beep("F4", time/8, "ones ")
        beep("E4", time/8, "who ")
        beep("D4", time/8, "are ")
        beep("C4", time/8, "dead.\n\n ")
        sleep(time/4)
        beep("A3", time/8, "But ")
        beep("Bb3", time/8, "there's ")
        beep("C4", time/4, "no ")
        beep("F4", time/4, "sense ")
        beep("E4", time/8, "cry")
        beep("D4", time/8, "ing\n ")
        beep("D4", time/8, "ov")
        beep("C4", time/8, "er ")
        beep("D4", time/8, "ev")
        beep("C4", time/8, "ery ")
        beep("C4", time/4, "mis")
        beep("C4", time/4, "take.\n ")
        beep("A3", time/8, "You ")
        beep("Bb3", time/8, "just ")
        beep("C4", time/4, "keep ")
        beep("F4", time/4, "on ")
        beep("G4", time/8, "try")
        beep("F4", time/8, "ing\n ")
        beep("E4", time/8, "till ")
        beep("D4", time/8, "you ")
        beep("D4", time/8, "run ")
        beep("E4", time/8, "out ")
        beep("F4", time/4, "of ")
        beep("F4", time/4, "cake.\n ")
        beep("G4", time/8, "And ")
        beep("A4", time/8, "the ")
        beep("Bb4", time/8, "Sci")
        beep("Bb4", time/8, "ence ")
        beep("A4", time/4, "gets ")
        beep("G4", time/4, "done.\n ")
        beep("F4", time/8, "and ")
        beep("G4", time/8, "you ")
        beep("A4", time/8, "make ")
        beep("A4", time/8, "a ")
        beep("G4", time/4, "neat ")
        beep("F4", time/4, "gun.\n ")
        beep("D4", time/8, "For ")
        beep("C4", time/8, "the ")
        beep("D4", time/8, "peo")
        beep("F4", time/8, "ple ")
        beep("F4", time/8, "who ")
        beep("E4", time/8 + time/8, "are\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n\n ")
        sleep(time/4 + time/2 + time + time + time/4 + time/8)
        clear()
        beep("A3", time/8, "I'm ")
        beep("G4", time/8, "not ")
        beep("F#4", time/8, "ev")
        beep("E4", time/8, "en ")
        beep("E4", time/8 + time/16, "an")
        beep("F#4", time/8 + time/16, "gry.\n ")
        sleep(time/4 + time/2 + time/2)
        beep("G4", time/8, "I'm ")
        beep("F#4", time/8, "be")
        beep("E4", time/8, "ing ")
        beep("E4", time/8 + time/4, "so ")
        beep("F#4", time/8, "sin")
        beep("D4", time/8 + time/8, "cere ")
        beep("E4", time/4, "right ")
        beep("A3", time/8 + time/8, "now.\n ")
        sleep(time/8 + time/4 + time/2)
        beep("E4", time/4, "Ev")
        beep("F#4", time/8, "en ")
        beep("G4", time/8 + time/4, "though ")
        beep("E4", time/4, "you ")
        beep("C#4", time/4, "broke ")
        beep("D4", time/8, "my ")
        beep("E4", time/8 + time/4, "heart.\n ")
        beep("A3", time/8, "And ")
        beep("A3", time/8 + time/8, "killed ")
        beep("F#4", time/8, "me.\n ")
        sleep(time/4 + time/2 + time/4 + time/8)
        beep("A3", time/8, "And ")
        beep("G4", time/8, "tore ")
        beep("F#4", time/8, "me ")
        beep("E4", time/8, "to ")
        beep("E4", time/8, "piec")
        beep("F#4", time/8, "es.\n ")
        sleep(time/8 + time/4 + time/2 + time/4 + time/8)
        beep("A3", time/8, "And ")
        beep("G4", time/8, "threw ")
        beep("F#4", time/8, "ev")
        beep("E4", time/8, "ery ")
        beep("E4", time/8, "piece ")
        sleep(time/4)
        beep("F#4", time/8, "in")
        beep("D4", time/8, "to ")
        sleep(time/4)
        beep("E4", time/8, "a ")
        beep("A3", time/8 + time/8, "fire.\n ")
        sleep(time/8 + time/4 + time/2)
        beep("E4", time/4, "As ")
        beep("F#4", time/8, "they ")
        beep("G4", time/8 + time/4, "burned ")
        beep("E4", time/4, "it ")
        beep("C#4", time/4, "hurt ")
        beep("D4", time/8, "be")
        beep("E4", time/8, "cause\n ")
        sleep(time/8)
        beep("A3", time/8, "I ")
        beep("D4", time/8, "was ")
        beep("E4", time/8, "so ")
        beep("F4", time/8, "hap")
        beep("E4", time/8, "py ")
        beep("D4", time/8, "for ")
        beep("C4", time/8, "you!\n ")
        sleep(time/4)
        beep("A3", time/8, "Now ")
        beep("Bb3", time/8, "these ")
        beep("C4", time/4, "points ")
        beep("F4", time/4, "of ")
        beep("E4", time/8, "da")
        beep("D4", time/8, "ta\n ")
        beep("D4", time/8, "make ")
        beep("C4", time/8, "a ")
        beep("D4", time/8, "beau")
        beep("C4", time/8, "ti")
        beep("C4", time/4, "ful ")
        beep("C4", time/4, "line.\n ")
        beep("A3", time/8, "And ")
        beep("Bb3", time/8, "we're ")
        beep("C4", time/4, "out ")
        beep("F4", time/4, "of ")
        beep("G4", time/8, "be")
        beep("F4", time/8, "ta.\n ")
        beep("E4", time/8, "We're ")
        beep("D4", time/8, "re")
        beep("D4", time/8, "leas")
        beep("E4", time/8, "ing ")
        beep("F4", time/4, "on ")
        beep("F4", time/4, "time.\n ")
        beep("G4", time/8, "So ")
        beep("A4", time/8, "I'm ")
        beep("Bb4", time/8, "GLaD. ")
        beep("Bb4", time/8, "I ")
        beep("A4", time/4, "got ")
        beep("G4", time/4, "burned.\n ")
        beep("F4", time/8, "Think ")
        beep("G4", time/8, "of ")
        beep("A4", time/8, "all ")
        beep("A4", time/8, "the ")
        beep("G4", time/8, "things ")
        beep("F4", time/8, "we ")
        beep("F4", time/4, "learned\n ")
        beep("D4", time/8, "for ")
        beep("C4", time/8, "the ")
        beep("D4", time/8, "peo")
        beep("F4", time/8, "ple ")
        beep("F4", time/8, "who ")
        beep("E4", time/8 + time/8, "are\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n ")
        sleep(time/4 + time/2 + time + time + time/2)
        clear()
        beep("G4", time/8, "Go ")
        beep("F#4", time/8, "ahead ")
        beep("E4", time/8, "and ")
        beep("E4", time/8 + time/8, "leave ")
        beep("F#4", time/8, "me.\n ")
        sleep(time/4 + time/2 + time/4 + time/8)
        beep("A3", time/8, "I ")
        beep("G4", time/8, "think ")
        beep("F#4", time/8, "I ")
        beep("E4", time/8, "pre")
        beep("E4", time/8, "fer ")
        sleep(time/4)
        beep("F#4", time/8, "to ")
        beep("D4", time/8, "stay ")
        sleep(time/4)
        beep("E4", time/8, "in")
        beep("A3", time/8 + time/8, "side.\n ")
        sleep(time/8 + time/4 + time/2)
        beep("E4", time/4, "May")
        beep("F#4", time/8, "be ")
        beep("G4", time/8 + time/4, "you'll ")
        beep("E4", time/4, "find ")
        beep("C#4", time/4, "some")
        beep("D4", time/8, "one ")
        beep("E4", time/8 + time/4, "else\n ")
        beep("A3", time/8, "to ")
        beep("A3", time/8 + time/8, "help ")
        beep("F#4", time/8, "you.\n ")
        sleep(time/4 + time/2 + time/2)
        beep("G4", time/8, "May")
        beep("F#4", time/8, "be ")
        beep("E4", time/8, "Black ")
        beep("E4", time/8 + time/8, "Me")
        beep("F#4", time/8, "sa...\n ")
        sleep(time/4 + time/2 + time/2)
        beep("G4", time/8, "THAT ")
        beep("F#4", time/8, "WAS ")
        beep("E4", time/8, "A ")
        beep("E4", time/8, "JOKE. ")
        sleep(time/4)
        beep("F#4", time/8, "")
        beep("D4", time/8, "")
        sleep(time/4)
        beep("E4", time/8, "FAT ")
        beep("A3", time/8 + time/8, "CHANCE.\n ")
        sleep(time/8 + time/4 + time/2)
        beep("E4", time/4, "An")
        beep("F#4", time/8, "y")
        beep("G4", time/8 + time/4, "way, ")
        beep("E4", time/4, "this ")
        beep("C#4", time/4, "cake ")
        beep("D4", time/8, "is ")
        beep("E4", time/8, "great.\n ")
        sleep(time/8)
        beep("A3", time/8, "It's ")
        beep("D4", time/8, "so ")
        beep("E4", time/8, "de")
        beep("F4", time/8, "lic")
        beep("E4", time/8, "ious ")
        beep("D4", time/8, "and ")
        beep("C4", time/8, "moist.\n ")
        sleep(time/4)
        beep("A3", time/8, "Look ")
        beep("Bb3", time/8, "at ")
        beep("C4", time/4, "me ")
        beep("F4", time/4, "still ")
        beep("E4", time/8, "talk")
        beep("D4", time/8, "ing\n ")
        beep("D4", time/8, "when ")
        beep("C4", time/8, "there's ")
        beep("D4", time/8, "Sci")
        beep("C4", time/8, "ence ")
        beep("C4", time/4, "to ")
        beep("C4", time/4, "do.\n ")
        beep("A3", time/8, "When ")
        beep("Bb3", time/8, "I ")
        beep("C4", time/4, "look ")
        beep("F4", time/4, "out ")
        beep("G4", time/8, "there,\n ")
        beep("F4", time/8, "it ")
        beep("E4", time/8, "makes ")
        beep("D4", time/8, "me ")
        beep("D4", time/8, "GLaD ")
        beep("E4", time/8, "I'm ")
        beep("F4", time/4, "not ")
        beep("F4", time/4, "you.\n ")
        beep("G4", time/8, "I've ")
        beep("A4", time/8, "ex")
        beep("Bb4", time/8, "per")
        beep("Bb4", time/8, "i")
        beep("A4", time/8, "ments ")
        beep("G4", time/8, "to ")
        beep("G4", time/4, "run.\n ")
        beep("F4", time/8, "There ")
        beep("G4", time/8, "is ")
        beep("A4", time/8, "re")
        beep("A4", time/8, "search ")
        beep("G4", time/8, "to ")
        beep("F4", time/8, "be ")
        beep("F4", time/4, "done.\n ")
        beep("D4", time/8, "On ")
        beep("C4", time/8, "the ")
        beep("D4", time/8, "peo")
        beep("F4", time/8, "ple ")
        beep("F4", time/8, "who ")
        beep("E4", time/8 + time/8, "are\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/4 + time/8, "live.")
        sleep(time/4 + time/4)
        clear()
        beep("A4", time/8, "PS: And ")
        beep("A4", time/8, "be")
        beep("B4", time/8, "lieve ")
        beep("A4", time/8, "me ")
        beep("F#4", time/8, "I ")
        beep("D4", time/8 + time/8, "am\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n ")
        sleep(time/4 + time/8)
        beep("A4", time/8, "PPS: I'm ")
        beep("A4", time/8, "do")
        beep("A4", time/8, "ing ")
        beep("B4", time/8, "sci")
        beep("A4", time/8, "ence ")
        beep("F#4", time/8, "and ")
        beep("D4", time/8 + time/8, "I'm\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n ")
        sleep(time/4 + time/8)
        beep("A4", time/8, "PPPS: I ")
        beep("A4", time/8, "feel ")
        beep("A4", time/8, "FAN")
        beep("B4", time/8, "TAS")
        beep("A4", time/8, "TIC ")
        beep("F#4", time/8, "and ")
        beep("D4", time/8 + time/8, "I'm\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n\n ")
        sleep(time/4 + time/4)
        beep("A4", time/8, "FINAL THOUGHT:\n While ")
        beep("A4", time/8, "you're ")
        beep("B4", time/8, "dy")
        beep("A4", time/8, "ing ")
        beep("F#4", time/8, "I'll ")
        beep("D4", time/8 + time/8, "be\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n\n ")
        sleep(time/4 + time/8)
        beep("A4", time/8, "FINAL THOUGHT PS:\n And ")
        beep("A4", time/8, "when ")
        beep("A4", time/8, "you're ")
        beep("B4", time/8, "dead ")
        beep("A4", time/8, "I ")
        beep("F#4", time/8, "will ")
        beep("D4", time/8 + time/8, "be\n ")
        beep("E4", time/8, "still ")
        beep("F#4", time/8, "a")
        beep("F#4", time/8 + time/4, "live.\n\n\n ")
        sleep(time/4 + time/8)
        beep("G4", time/8, "STILL ")
        beep("A4", time/8, "A")
        beep("A4", time/8 + time/4, "LIVE.")
        sleep(time/4)
        clear()
        sleep(time/8)
        beep("G4", time/8, "")
        beep("F#4", time/8, "")
        beep("F#4", time/8 + time/4, "")
        sleep(time/4 + time/2)
        clear()
        0  // Exit code
