with Ada.Text_IO,
     Ada.Command_Line;

procedure Nine_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    Line_Length : constant Natural := 100;

    type Depth is range 0..9;
    type Line is array(1..Line_Length) of Depth;

    type Line_Access is access Line;
    type Raster is array(1..3) of Line_Access;
    current_raster : Raster := (others => null);

    line_id : Natural := 0;

    file : Text_IO.File_Type;

    procedure Parse_Scanline(raw_line : in String;
                             scanline : in out Line_Access) is
    begin
        for I in Line'Range loop
            scanline(I) := Depth(Character'Pos(raw_line(I)) - Character'Pos('0'));
        end loop;
    end Parse_Scanline;

    nr_points : Natural := 0;

    function Find_Depths(rast : in Raster) return Natural is
        line_min, col_min : Boolean := false;
        risk : Natural := 0;
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
                risk := risk + Natural(rast(2)(I)) + 1;
                nr_points := nr_points + 1;
            end if;
        end loop;

        return risk;
    end Find_Depths;

    temp_line : Line_Access := null;
    all_risk : Natural := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 9.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file of depths. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    current_raster(3) := new Line;
    Parse_Scanline(Text_IO.Get_Line(file), current_raster(3));

    while not Text_IO.End_Of_File(file) loop
        current_raster(1..2) := current_raster(2..3);
        current_raster(3) := new Line;
        Parse_Scanline(Text_IO.Get_Line(file), current_raster(3));

        -- Process the current scanline
        all_risk := all_risk + Find_Depths(current_raster);

        line_id := line_id + 1;
    end loop;

    current_raster(1..2) := current_raster(2..3);
    current_raster(3) := null;
    all_risk := all_risk + Find_Depths(current_raster);

    Text_IO.Put_Line("Risk value: " & Natural'Image(all_risk));
    Text_IO.Put_Line("Found " & Natural'Image(nr_points) & " deep points.");

end Nine_First;
