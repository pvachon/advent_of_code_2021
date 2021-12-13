with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Hashed_Sets,
     Ada.Strings.Fixed;

use Ada.Containers,
    Ada.Strings.Fixed;

procedure Thirteen_Second is
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

    max_x : Natural := 0;
    max_y : Natural := 0;

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

        max_x := 0;
        max_y := 0;

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
                if new_point.Y > max_y then
                    max_y := new_point.Y;
                end if;

                if new_point.X > max_x then
                    max_x := new_point.X;
                end if;
                new_active.Insert(New_Item => new_point,
                                  Position => location,
                                  Inserted => inserted);
            end Try_Insert_Point;
        end loop;

        active_points := new_active;
    end Do_Fold;

    procedure Plot_Points(iter : in Natural) is
        use Point_Set;
        out_image : Text_IO.File_Type;
    begin
        Text_IO.Create(File => out_image,
                       Mode => Text_IO.Out_File,
                       Name => "step_" & Natural'Image(iter) & ".pbm");

        Text_IO.Put_Line(out_image, "P1");
        Text_IO.Put_Line(out_image, Natural'Image(max_x + 1) & " " & Natural'Image(max_y + 1));

        for r_Y in 0..max_y loop
            for r_X in 0..max_x loop
                Check_Point : declare
                    c : Point_Set.Cursor := Point_Set.No_Element;
                begin
                    c := active_points.Find((X => r_X, Y => r_Y));
                    if c = Point_Set.No_Element then
                        Text_IO.Put(out_image, "0 ");
                    else
                        Text_IO.Put(out_image, "1 ");
                    end if;

                end Check_Point;
            end loop;
            Text_IO.New_Line(out_image);
        end loop;
    end Plot_Points;

    iter : Natural := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 13.1");

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
        iter := iter + 1;
        Do_Fold(Text_IO.Get_Line(file));
        Plot_Points(iter);
    end loop;

end Thirteen_Second;
