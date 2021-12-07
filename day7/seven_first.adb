with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Strings.Fixed;

procedure Seven_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;
    package SF renames Ada.Strings.Fixed;

    file : Text_IO.File_Type;

    package Crabs is new Ada.Containers.Vectors(
        Element_Type => Natural,
        Index_Type => Natural);

    all_crabs : Crabs.Vector;

    minimum, maximum : Natural := 0;

    procedure Parse_Crabs(line : in String) is
        count : Natural := 0;
        next_comma : Natural;
        start : Natural := line'First;
        is_first : Boolean := True;
        current_crab : Natural;
    begin
        count := SF.Count(Source => line,
                          Pattern => ",") + 1;

        all_crabs.Reserve_Capacity(Ada.Containers.Count_Type(count));

        for I in 1..count loop
            next_comma := SF.Index(Source => line(start..line'Last),
                                   Pattern => ",");
            if next_comma = 0 then
                next_comma := line'Last + 1;
            end if;

            current_crab := Natural'Value(line(start..next_comma - 1));
            all_crabs.Append(current_crab);

            if is_first then
                minimum := current_crab;
                maximum := current_crab;
                is_first := False;
            else
                if minimum > current_crab then
                    minimum := current_crab;
                end if;

                if maximum < current_crab then
                    maximum := current_crab;
                end if;
            end if;
            start := next_comma + 1;
        end loop;
    end Parse_Crabs;

    first_trial : Boolean := true;
    target_square, total_fuel : Natural := 0;

    function Trial(target_loc : in Natural) return Natural is
        fuel : Natural := 0;
    begin
        for crab of all_crabs loop
            fuel := fuel + abs(crab - target_loc);
        end loop;

        return fuel;
    end Trial;

    fuel_used : Natural := 0;

begin

    Text_IO.Put_Line("Advent of Code 2021: 7.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        Parse_Crabs(Text_IO.Get_Line(file));
    end loop;

    Text_IO.Put_Line("Number of crabs: " & Ada.Containers.Count_Type'Image(all_crabs.Length) &
        " minimum: " & Natural'Image(minimum) & ", maximum: " & Natural'Image(maximum));

    for I in minimum..maximum loop
        fuel_used := Trial(I);
        if first_trial then
            target_square := I;
            total_fuel := fuel_used;
            first_trial := False;
        else
            if total_fuel > fuel_used then
                total_fuel := fuel_used;
                target_square := I;
            end if;
        end if;
    end loop;

    Text_IO.Put_Line("Target square is " & Natural'Image(target_square) & " fuel consumed is " &
                     Natural'Image(total_fuel));

end Seven_First;
