with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors;

procedure Nine_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    Line_Length : constant Natural := 100;
    Scanlines : constant Natural := 100;
    --Line_Length : constant Natural := 10;
    --Scanlines : constant Natural := 5;

    type Depth is range 0..9;
    type Line is array(1..Line_Length) of Depth;

    type Line_Access is access all Line;
    type Raster is array(1..3) of Line_Access;
    current_raster : Raster := (others => null);

    type Image is array(1..Scanlines) of aliased Line;
    img : Image;

    type Point is record
        x, y : Natural;
    end record;

    package Points is new Ada.Containers.Vectors(
        Element_Type => Point,
        Index_Type => Natural);
    pts : Points.Vector;

    line_id : Natural := 0;

    file : Text_IO.File_Type;

    procedure Parse_Scanline(raw_line : in String;
                             scanline : in out Line) is
    begin
        for I in Line'Range loop
            scanline(I) := Depth(Character'Pos(raw_line(I)) - Character'Pos('0'));
        end loop;
    end Parse_Scanline;

    procedure Find_Depths(rast : Raster;
                          line_number : Natural) is
        line_min, col_min : Boolean := false;
    begin
        for I in Line'Range loop
            line_min := false;
            if I = Line'First then
                -- Check only to the right
                line_min := rast(2)(I + 1) > rast(2)(I);
            elsif I = Line'Last then
                -- Check only to the left
                line_min := rast(2)(I - 1) > rast(2)(I);
            else
                -- Check left and right
                line_min := rast(2)(I - 1) > rast(2)(I) and rast(2)(I + 1) > rast(2)(I);
            end if;

            col_min := false;
            if null = rast(1) then
                col_min := rast(3)(I) > rast(2)(I);
            elsif null = rast(3) then
                col_min := rast(1)(I) > rast(2)(I);
            else
                col_min := rast(1)(I) > rast(2)(I) and rast(3)(I) > rast(2)(I);
            end if;

            if col_min and line_min then
                pts.Append((x => I, y => line_number));
            end if;
        end loop;
    end Find_Depths;

    function Flood_Fill(img : in out Image;
                        pt : in Point) return Natural is
        count : Natural := 1;
    begin
        if img(pt.y)(pt.x) = 9 then
            return 0;
        end if;

        img(pt.y)(pt.x) := 9;

        if pt.x /= Line'Last then
            count := count + Flood_Fill(img, (x => pt.x + 1, y => pt.y));
        end if;

        if pt.x /= Line'First then
            count := count + Flood_Fill(img, (x => pt.x - 1, y => pt.y));
        end if;

        if pt.y /= Image'First then
            count := count + Flood_Fill(img, (x => pt.x, y => pt.y - 1));
        end if;

        if pt.y /= Image'Last then
            count := count + Flood_Fill(img, (x => pt.x, y => pt.y + 1));
        end if;

        return count;
    end Flood_Fill;

    product : Natural := 1;
    fill : Natural;

    package Fill_Counts is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_type => Natural);
    fills : Fill_Counts.Vector;

    package Fill_Sorter is new Fill_Counts.Generic_Sorting;
begin

    Text_IO.Put_Line("Advent of Code 2021: 9.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file of depths. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        line_id := line_id + 1;
        Parse_Scanline(Text_IO.Get_Line(file), img(line_id));
    end loop;

    current_raster(3) := img(Image'First)'Access;

    for I in Image'First + 1..Image'Last loop
        current_raster(1..2) := current_raster(2..3);
        current_raster(3) := img(I)'Access;

        -- Process the current scanline
        Find_Depths(current_raster, I - 1);
    end loop;

    current_raster(1..2) := current_raster(2..3);
    current_raster(3) := null;
    Find_Depths(current_raster, img'Last);

    Text_IO.Put_Line("There are " & Ada.Containers.Count_Type'Image(pts.Length));

    fills.Reserve_Capacity(pts.Length);

    for P of pts loop
        fill := Flood_Fill(img, P);
        Text_IO.Put_Line("  Flooded (" & Natural'Image(P.x) & "," & Natural'Image(P.y) & ")" & Natural'Image(fill) & " points");
        fills.append(fill);
    end loop;

    Fill_Sorter.Sort(fills);
    fills.Reverse_Elements;

    Text_IO.Put_Line("Product of fills is " & Natural'Image(fills(0) * fills(1) * fills(2)));
end Nine_Second;
