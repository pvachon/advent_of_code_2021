with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings.Fixed,
     Ada.Containers.Vectors;

use Ada.Strings.Fixed;

procedure Eight_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    type Display_Segment is range 1..7;

    type Display_Value is array (Display_Segment) of Boolean;

    type States is array(1..10) of Display_Value;
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

    function Parse_Line(line : String) return Reading is
        pipe : Natural := 0;
        disp : Displayed;
    begin

        pipe := Index(Source => line,
                      Pattern => "|");

        Parse_Displayed(line(pipe + 2..line'Last), disp);

        return (all_states => (others => (others => False)), all_displayed => disp);
    end Parse_Line;

    count_1478 : Natural := 0;

begin

    Text_IO.Put_Line("Advent of Code 2021: 8.0");

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

    Text_IO.Put_Line("Got " & Ada.Containers.Count_Type'Image(all_readings.Length) & " items.");

    for r of all_readings loop
        for d of r.all_displayed loop
            declare
                segment_count : Natural := Count_Segments_Set(d);
            begin
                case segment_count is
                    when 2 | 3 | 4 | 7 => count_1478 := count_1478 + 1;
                    when others => null;
                end case;
            end;
        end loop;
    end loop;

    Text_IO.Put_Line("There are " & Natural'Image(count_1478) & " 1, 4, 7 or 8 values.");

end Eight_First;
