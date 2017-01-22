open System

module TicTacToe =
    /// 'Settings'
    let conf_symbol_p1 = "X"
    let conf_symbol_p2 = "O"
    let conf_symbol_empty = " "

    /// Prints red text prefixed with "Error: "
    let printerr (str:string) =
        Console.ForegroundColor <- ConsoleColor.DarkRed
        printf " Error: %s\n\n\n" str
        Console.ForegroundColor <- ConsoleColor.Gray

    /// Prints blue friendly text
    let printnot (str:string) =
        Console.ForegroundColor <- ConsoleColor.DarkCyan
        printf " %s\n\n\n" str
        Console.ForegroundColor <- ConsoleColor.Gray


    /// Resets some settings and prints title screen
    let title(str:string) =
        Console.Clear()
        Console.WindowWidth <- 70
        Console.BufferWidth <- 70
        Console.WindowHeight <- 25
        Console.BufferHeight <- 25
        Console.ForegroundColor <- ConsoleColor.Gray
        Console.Title <- "Tic Tac Toe"

        let title = "  _____   _            _____                    _____                \n"
                  + " |_   _| (_)   ___    |_   _|   __ _    ___    |_   _|   ___     ___ \n"
                  + "   | |   | |  / __|     | |    / _` |  / __|     | |    / _ \   / _ \\\n"
                  + "   | |   | | | (__      | |   | (_| | | (__      | |   | (_) | |  __/\n"
                  + "   |_|   |_|  \___|     |_|    \__,_|  \___|     |_|    \___/   \___|\n"

        printf "%s\n\n\n" title
        if (str = "error") then
            printerr("Invalid input!")
        else
            printnot("Format: ROW + COLUMN. Example: A1 (first row, first column)")


     /// Returns what symbol an int represents (defined by conf_symbol_xxx)
    let get_symbol(value:int) = if value = 0 then conf_symbol_empty else if value = 1 then conf_symbol_p1 else if value = 2 then conf_symbol_p2 else string(value)

    /// Given an array, prints that array as a board
    let print_board(arr:array<int>) =
        let space = "                          "
        let top = "    1   2   3 \n\n"
        let mid = "   -----------\n"
        printf "%s%s%sA   %s | %s | %s \n%s%s" space top space (get_symbol(arr.[0])) (get_symbol(arr.[1])) (get_symbol(arr.[2])) space mid
        printf "%sB   %s | %s | %s \n%s%s" space (get_symbol(arr.[3])) (get_symbol(arr.[4])) (get_symbol(arr.[5])) space mid
        printf "%sC   %s | %s | %s \n" space (get_symbol(arr.[6])) (get_symbol(arr.[7])) (get_symbol(arr.[8]))

    /// Given a char + int combination, returns that position in a 3x3 array, or returns 99
    let get_board_position(str:string) =
        if (int(str.[0]) - 64) < 4 then
            ((int(string(str.[1])) - 1) + (int(str.[0]) - 65) * 3)
        else
            99

    /// Updates given board with a given position and player number
    let build_board(arr:array<int>, str:string, player:int) =
        let pos = get_board_position(str)
        //printnot (string(pos)) // print the position on the array
        let new_board = [|
            for i = 0 to arr.Length-1 do
                if i = pos && arr.[i] = 0 then
                    yield player
                else
                    yield arr.[i]
        |]
        new_board

    /// Checks if input follows (char)(int) format
    let validate_input(str:string) =
        let arr = [| for i in 'A' .. 'C' do for j in 1 .. 3 do yield string(i) + string(j) |]
        Array.contains str arr

    /// Checks if the selected cell hasn't been selected before
    let validate_position(arr: array<int>, str:string) =
        let pos = get_board_position(str)
        (arr.[pos] = 0)

    /// True if all items in the array are equal
    let rec all_equal(arr:array<int>) =
        if arr.Length >= 2 then
            if arr.[0] <> arr.[1] then
                false
            else
                all_equal(arr.[1..])
        else
            true

    /// Checks if there is any 3 cell pattern (a.k.a. victory)
    let check_victory(arr:array<int>) =
        (* 0 3 6 | 1 4 7 | 2 5 8 | 0 4 8 | 2 4 6 | 0 1 2 | 3 4 5 | 6 7 8 *)
        if (all_equal([|arr.[0]; arr.[3]; arr.[6]|]) && arr.[0] <> 0
        || all_equal([|arr.[0]; arr.[1]; arr.[2]|]) && arr.[1] <> 0
        || all_equal([|arr.[2]; arr.[4]; arr.[6]|]) && arr.[2] <> 0
        || all_equal([|arr.[3]; arr.[4]; arr.[5]|]) && arr.[3] <> 0
        || all_equal([|arr.[0]; arr.[4]; arr.[8]|]) && arr.[4] <> 0
        || all_equal([|arr.[2]; arr.[5]; arr.[8]|]) && arr.[5] <> 0
        || all_equal([|arr.[6]; arr.[7]; arr.[8]|]) && arr.[6] <> 0
        || all_equal([|arr.[1]; arr.[4]; arr.[7]|]) && arr.[7] <> 0) then
            true
        else
            false

    /// Game
    let rec game(state:string, board:array<int>, player:int) =
        if (state = "game") then
            print_board(board)
            printf "\n\n\n Player %i, pick a position to place (%A) > " player (get_symbol(player))

            let x = Console.ReadLine().ToUpper()

            if (validate_input(x) && validate_position(board, x)) then
                let next_board = build_board(board, x, player)
                if (check_victory(next_board)) then
                    title("")
                    game("end", next_board, player)
                else
                    title("")
                    game(state, build_board(board, x, player), (if player = 1 then 2 else 1))
            else
                title("error")
                game(state, board, player)

        else if (state = "end") then
            print_board(board)
            printf "\n\n\n Player %i wins!\n\n" player

    /// Between games
    let rec play(time:int) =
        if (time <> 1) then
            printf " Play again? (Y / N) > "
            let k = Console.ReadKey().KeyChar.ToString().ToUpper()
            if (k = "Y") then
                title("")
                game("game", [| for i in 1 .. 9 -> 0 |], 1)
                play(time + 1)
            else
                Console.Clear()
        else
            title("")
            game("game", [| for i in 1 .. 9 -> 0 |], 1)
            play(time + 1)

    /// Initial thread
    [<EntryPoint>]
    let main argv =
        play(1)
        0  // Exit code
