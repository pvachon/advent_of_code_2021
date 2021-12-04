with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings.Fixed,
     Ada.Containers.Vectors;

procedure Fourth_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;
    package SF renames Ada.Strings.Fixed;

    file : Text_IO.File_Type;

    package Draws is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_Type => Natural);
    game_draws : Draws.Vector;

    type Bingo_Card is array (1..5, 1..5) of Integer;

    package Bingo_Cards is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_Type => Bingo_Card);
    cards : Bingo_Cards.Vector;

    procedure Read_Card is
        card : Bingo_Card;

        line : String(1..14);
        start : Natural := 1;
    begin
        Text_IO.Skip_Line(file);

        for I in 1..5 loop
            line := Text_IO.Get_Line(file);
            start := 1;
            for J in 1..5 loop
                card(I, J) := Natural'Value(line(start..start+1));
                start := start + 3;
            end loop;
        end loop;

        cards.Append(card);
    end Read_Card;

    procedure Parse_Game_Draws(line : String) is
        count : Natural := 0;
        next_comma : Natural;
        start : Natural := line'First;
    begin
        count := sf.Count(Source => line,
                          Pattern => ",") + 1;

        Text_IO.Put_Line("There are " & Natural'Image(count) & " numbers to be pulled");

        game_draws.Reserve_Capacity(Ada.Containers.Count_Type(count));

        for I in 1..count loop
            next_comma := SF.Index(Source => line(start..line'Last),
                                   Pattern => ",");
            if next_comma = 0 then
                next_comma := line'Last + 1;
            end if;

            -- Text_IO.Put_Line("  Value: " & line(start..next_comma - 1) & " from: " & Natural'Image(start) & " to " & Natural'Image(next_comma));

            game_draws.Append(Natural'Value(line(start..next_comma - 1)));
            start := next_comma + 1;
        end loop;
    end Parse_Game_Draws;

    function Check_Winner(card : Bingo_Card) return Boolean is
        count : Natural := 0;
    begin
        for I in 1..5 loop
            count := 0;
            for J in 1..5 loop
                if card(I, J) /= -1 then
                    exit;
                end if;
                count := count + 1;
            end loop;

            if count = 5 then
                return True;
            end if;

            count := 0;
            for J in 1..5 loop
                if card(J, I) /= -1 then
                    exit;
                end if;
                count := count + 1;
            end loop;

            if count = 5 then
                return True;
            end if;
        end loop;

        return False;
    end Check_Winner;

    function Get_Sum(card : Bingo_Card) return Natural is
        sum : Natural := 0;
    begin
        for I in 1..5 loop
            for J in 1..5 loop
                if card(I, J) /= -1 then
                    sum := sum + card(I, J);
                end if;
            end loop;
        end loop;

        return sum;
    end Get_Sum;
begin

    Text_IO.Put_Line("Advent of Code 2021: 4.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify input bingo game. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Parse_Game_Draws(Text_IO.Get_Line(file));

    Text_IO.Put_Line("Parsing Bingo Cards...");

    while not Text_IO.End_Of_File(file) loop
        Read_Card;
    end loop;

    Text_IO.Put_Line("There are " & Ada.Containers.Count_Type'Image(cards.Length) & " cards.");

    for draw of game_draws loop
        Text_IO.Put_Line("Number drawn: " & Natural'Image(draw));
        for card of cards loop
            for I in 1..5 loop
                for J in 1..5 loop
                    if card(I, J) = draw then
                        card(I, J) := -1;
                        exit;
                    end if;
                end loop;
            end loop;

            -- Check the card
            if Check_Winner(card) then
                Text_IO.Put_Line("We have a winner! Magic is: " & Natural'Image(Get_Sum(card) * draw));
                return;
            end if;
        end loop;

    end loop;

end Fourth_First;
