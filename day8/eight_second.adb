with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings.Fixed,
     Ada.Containers.Vectors;

use Ada.Strings.Fixed;

procedure Eight_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    type Display_Segment is range 1..7;

    type Display_Value is array (Display_Segment) of Boolean;

    type States is array(0..9) of Display_Value;
    type Displayed is array(1..4) of Display_Value;

    type Reading is record
        all_states : States;
        all_displayed : Displayed;
    end record;

    package Readings is new Ada.Containers.Vectors(
        Element_Type => Reading,
        Index_Type => Natural);
    all_readings : Readings.Vector;

    function Count_Segments_Set(value : Display_Value) return Natural is
        counter : Natural := 0;
    begin
        for I in Display_Value'Range loop
            counter := (if value(I) = True then counter + 1 else counter);
        end loop;

        return counter;
    end Count_Segments_Set;

    function Parse_Display_Value(input : String) return Display_Value is
        tmp : Display_Value := (others => False);
        idx : Display_Segment;
    begin
        for C of input loop
            idx := Display_Segment(Character'Pos(C) - 97 + 1);
            tmp(idx) := True;
        end loop;

        return tmp;
    end Parse_Display_Value;

    procedure Parse_Displayed(line : String;
                              disp : in out Displayed) is
        space, prev_space : Natural := line'First;
    begin
        for I in 1..4 loop
            space := Index(Source => line(prev_space..line'Last), Pattern => " ");
            space := (if space = 0 then line'Last else space - 1);
            disp(I) := Parse_Display_Value(line(prev_space..space));
            prev_space := space + 2;
        end loop;
    end Parse_Displayed;

    procedure Parse_Truth_Table(line : String;
                                all_states : in out States) is
        last_space : Natural := line'First;
        space : Natural;
    begin
        for I in States'Range loop
            space := Index(Source => line(last_space..line'Last), Pattern => " ");
            all_states(I) := Parse_Display_Value(line(last_space..space - 1));
            last_space := space + 1;
        end loop;
    end Parse_Truth_Table;

    function Count_Overlapping(st1, st2 : in Display_Value) return Natural is
        count : Natural := 0;
    begin
        for I in Display_Value'Range loop
            if true = (st1(I) and st2(I)) then
                count := count + 1;
            end if;
        end loop;

        return count;
    end Count_Overlapping;

    function Parse_Line(line : String) return Reading is
        pipe : Natural := 0;
        disp : Displayed;
        st_raw, st_sorted : States;

        count_1, count_4, count_7 : Natural;
    begin

        pipe := Index(Source => line,
                      Pattern => "|");

        -- Parse the records
        Parse_Truth_Table(line(line'First..pipe - 1), st_raw);
        Parse_Displayed(line(pipe + 2..line'Last), disp);

        -- Sort the trivial states
        for I in st_raw'Range loop
            case Count_Segments_Set(st_raw(I)) is
                when 2 => st_sorted(1) := st_raw(I);
                when 3 => st_sorted(7) := st_raw(I);
                when 4 => st_sorted(4) := st_raw(I);
                when 7 => st_sorted(8) := st_raw(I);
                when 5 | 6 => null;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end loop;

        -- Identify the other values
        for I in st_raw'Range loop
            count_1 := Count_Overlapping(st_sorted(1), st_raw(I));
            count_4 := Count_Overlapping(st_sorted(4), st_raw(I));
            count_7 := Count_Overlapping(st_sorted(7), st_raw(I));
            case Count_Segments_Set(st_raw(I)) is
                when 5 => -- Could be 2, 3 or 5
                    if count_4 = 2 then
                        st_sorted(2) := st_raw(I);
                    elsif count_1 = 2 then
                        st_sorted(3) := st_raw(I);
                    else
                        st_sorted(5) := st_raw(I);
                    end if;
                when 6 => -- Could be 6, 9 or 0
                    if count_1 = 1 then
                        st_sorted(6) := st_raw(I);
                    elsif count_4 = 4 then
                        st_sorted(9) := st_raw(I);
                    else
                        st_sorted(0) := st_raw(I);
                    end if;
                when 2 | 3 | 4 | 7 => null;
                when others => raise Ada.Strings.Index_Error;
            end case;
        end loop;

        return (all_states => st_sorted, all_displayed => disp);
    end Parse_Line;

    function Find_Digit(displayed : Display_Value;
                        all_states : States) return Natural is
    begin
        for I in States'Range loop
            if displayed = all_states(I) then
                return I;
            end if;
        end loop;

        raise Ada.Strings.Index_Error;
    end Find_Digit;

    count, total : Natural := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 8.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem input. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        all_readings.Append(Parse_Line(Text_IO.Get_Line(file)));
    end loop;

    for reading of all_readings loop
        count := 0;
        for I in Displayed'Range loop
            count := count * 10 + Find_Digit(reading.all_displayed(I), reading.all_states);
        end loop;
        total := total + count;
    end loop;

    Text_IO.Put_Line("Total is " & Natural'Image(total));

end Eight_Second;
