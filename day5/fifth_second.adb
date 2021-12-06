with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings,
     Ada.Strings.Fixed,
     Ada.Containers.Vectors;

procedure Fifth_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;
    package SF renames Ada.Strings.Fixed;

    Max_Coordinate : constant Natural := 999;

    type Coordinate_Element is range 0..Max_Coordinate;

    file : Text_IO.File_Type;

    type Point is record
        x, y : Coordinate_Element;
    end record;

    type Line is record
        start, finish : Point;
    end record;

    function "<" (Left, Right : Line) return Boolean is
    begin
        if Left.start.y < Right.start.y then
            return True;
        else
            return False;
        end if;
    end "<";

    package Lines is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_Type => Line);
    all_lines : Lines.Vector;

    package Lines_Sorter is new Lines.Generic_Sorting;

    type Scanline is array (Coordinate_Element) of Natural;

    current_scanline : Scanline;

    multiple_covering : Natural := 0;

    procedure Clear_Scanline is begin
        for I in Coordinate_Element'Range loop
            current_scanline(I) := 0;
        end loop;
    end Clear_Scanline;

    function Parse_Point(raw_point : String) return Line is
        raw_x1, raw_x2, raw_y1, raw_y2 : Coordinate_Element := 0;
        slice_start, slice_end : Natural;
    begin
        slice_end := SF.Index(Source => raw_point,
                              Pattern => ",");
        raw_x1 := Coordinate_Element'Value(raw_point(raw_point'First..slice_end - 1));

        slice_start := slice_end + 1;
        slice_end := SF.Index(Source => raw_point(slice_start..raw_point'Last),
                              Pattern => " ");

        raw_y1 := Coordinate_Element'Value(raw_point(slice_start..slice_end - 1));

        slice_start := slice_end + 3;
        slice_end := SF.Index(Source => raw_point(slice_start..raw_point'Last),
                              Pattern => ",");

        raw_x2 := Coordinate_Element'Value(raw_point(slice_start..slice_end - 1));

        raw_y2 := Coordinate_Element'Value(raw_point(slice_end+1..raw_point'Last));

        -- Sort all lines so they start with the smaller y
        if raw_y1 < raw_y2 then
            return (start => (x => raw_x1, y => raw_y1), finish => (x => raw_x2, y => raw_y2));
        else
            return (start => (x => raw_x2, y => raw_y2), finish => (x => raw_x1, y => raw_y1));
        end if;
    end Parse_Point;

begin

    Text_IO.Put_Line("Advent of Code 2021: 5.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        all_lines.Append(Parse_Point(Text_IO.Get_Line(file)));
    end loop;

    Lines_Sorter.Sort(all_lines);

    for I in Coordinate_Element'Range loop
        Clear_Scanline;
        for line of all_lines loop
            if line.start.y <= I and line.finish.y >= I then
                if line.start.x = line.finish.x or line.start.y = line.finish.y then
                    declare
                        x1, x2 : Coordinate_Element;
                    begin
                        if line.start.x > line.finish.x then
                            x1 := line.finish.x;
                            x2 := line.start.x;
                        else
                            x1 := line.start.x;
                            x2 := line.finish.x;
                        end if;

                        for J in x1..x2 loop
                            current_scanline(J) := current_scanline(J) + 1;
                            multiple_covering := (if current_scanline(J) = 2
                                then multiple_covering + 1
                                else multiple_covering);
                        end loop;
                    end;
                else
                    -- Diagonal line
                    declare
                        intercept : Coordinate_Element;
                    begin
                        if line.start.x < line.finish.x then
                            intercept := line.start.x + (I - line.start.y);
                        else
                            intercept := line.start.x - (I - line.start.y);
                        end if;

                        current_scanline(intercept) := current_scanline(intercept) + 1;
                        multiple_covering := (if current_scanline(intercept) = 2
                            then multiple_covering + 1
                            else multiple_covering);
                    end;
                end if;
            end if;
        end loop;
    end loop;

    Text_IO.Put_Line("Multiple covering line segments: " & Natural'Image(multiple_covering));

end Fifth_Second;
