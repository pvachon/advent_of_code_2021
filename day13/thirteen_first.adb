with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Hashed_Sets,
     Ada.Strings.Fixed;

use Ada.Containers,
    Ada.Strings.Fixed;

procedure Thirteen_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    type Point is record
        X, Y : Natural;
    end record;

    function "="(Left, Right : in Point) return Boolean is begin
        return Left.X = Right.X and Left.Y = Right.Y;
    end "=";

    function Hash(Element : in Point) return Ada.Containers.Hash_Type is
        M11 : constant := 2047;
    begin
        return Hash_Type(Element.X) + Hash_Type(Element.Y) * M11;
    end Hash;

    package Point_Set is new Ada.Containers.Hashed_Sets(
        Element_Type => Point,
        Hash => Hash,
        Equivalent_Elements => "="
        );
    active_points : Point_Set.Set;

    procedure Read_Dots is
        function Parse_Coordinate(line : String) return Boolean is
            comma : Natural := Index(Source => line, Pattern => ",");
            x, y : Natural;
        begin
            if comma = 0 then
                return False;
            end if;

            x := Natural'Value(line(line'First..comma - 1));
            y := Natural'Value(line(comma + 1..line'Last));

            active_points.Insert((X => x, Y => y));

            return True;
        end Parse_Coordinate;
    begin
        loop
            if not Parse_Coordinate(Text_IO.Get_Line(file)) then
                exit;
            end if;
        end loop;

        Text_IO.Put_Line("Read in " & Ada.Containers.Count_Type'Image(active_points.Length));
    end Read_Dots;

    procedure Do_Fold(line : in String) is
        equals : Natural := Index(Source => line, Pattern => "=");
        fold_at : Natural;
        new_active : Point_Set.Set;
        new_point : Point;
    begin
        if 0 = equals then
            raise Ada.Strings.Index_Error;
        end if;

        fold_at := Natural'Value(line(equals + 1..line'Last));

        Text_IO.Put_Line("Folding in " & line(equals - 1) & " axis at " & Natural'Image(fold_at));

        for pt of active_points loop
            new_point := pt;
            case line(equals - 1) is
                when 'y' =>
                    if new_point.Y > fold_at then
                        new_point.Y := fold_at - (new_point.Y - fold_at);
                    end if;
                when 'x' => null;
                    if new_point.X > fold_at then
                        new_point.X := fold_at - (new_point.X - fold_at);
                    end if;
                when others => raise Ada.Strings.Index_Error;
            end case;

            Try_Insert_Point: declare
                inserted : Boolean := false;
                location : Point_Set.Cursor := Point_Set.No_Element;
            begin
                new_active.Insert(New_Item => new_point,
                                  Position => location,
                                  Inserted => inserted);
            end Try_Insert_Point;
        end loop;

        active_points := new_active;

        Text_IO.Put_Line("There are " & Ada.Containers.Count_Type'Image(new_active.Length));

    end Do_Fold;
begin

    Text_IO.Put_Line("Advent of Code 2021: 13.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Read_Dots;

    while not Text_IO.End_Of_File(file) loop
        Do_Fold(Text_IO.Get_Line(file));
    end loop;

end Thirteen_First;
