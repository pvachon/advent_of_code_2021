with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Ordered_Maps,
     Sparse_Raster;

use Sparse_Raster;

procedure Twenty_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    type Mapping is range 0..512;
    type Filter is array(Mapping) of Boolean;

    filt : Filter;

    file : Text_IO.File_Type;

    raster : Sparse_Raster.Raster;

    procedure Load_Mapping(line : String) is
        mapping_offs : Mapping := 0;
    begin
        for I of line loop
            filt(mapping_offs) := (if I = '#' then true else false);
            mapping_offs := mapping_offs + 1;
        end loop;
    end Load_Mapping;

    procedure Load_Raster
    is
        I : Sparse_Raster.Pixel_Offset := 0;
    begin
        while not Text_IO.End_Of_File(file) loop
            Read_Line : declare
                line : String := Text_IO.Get_Line(file);
                x : Sparse_Raster.Pixel_Offset := 0;
            begin
                for C of line loop
                    if C = '#' then
                        raster.Set_Pixel(x, I);
                    end if;
                    x := x + 1;
                end loop;
            end Read_Line;
            I := I + 1;
        end loop;
        Text_IO.Put_Line("Loaded raster of length " & Pixel_Offset'Image(I));
    end Load_Raster;

    procedure Do_Iteration is
        new_raster : Sparse_Raster.Raster;
        neigh : Neighbourhood;
    begin
        Text_IO.Put_Line("Input raster range: (" & Pixel_Offset'Image(raster.x_min) & ","
            & Pixel_Offset'Image(raster.y_min) & ") -> (" & Pixel_Offset'Image(raster.x_max)
            & "," & Pixel_Offset'Image(raster.y_max) & ")");
        for J in raster.y_min - 1..raster.y_max + 1 loop
            for I in raster.x_min - 1..raster.x_max + 1 loop
                neigh := raster.Get_Neighbourhood(I, J);
                if filt(Mapping(neigh)) then
                    new_raster.Set_Pixel(I, J);
                end if;
            end loop;
        end loop;
        new_raster.Update_Bounds(raster.y_max - 1, raster.y_min - 1);
        new_raster.Update_Bounds(raster.x_max + 1, raster.y_max + 1);
        raster := new_raster;
    end Do_Iteration;

    procedure Dump_Raster is begin
        for J in raster.y_min..raster.y_max loop
            for I in raster.x_min..raster.x_max loop
                if raster.Get_Pixel(I, J) then
                    Text_IO.Put("#");
                else
                    Text_IO.Put(".");
                end if;
            end loop;
            Text_IO.New_Line;
        end loop;
    end Dump_Raster;

    filter_prev : Boolean;
begin

    Text_IO.Put_Line("Advent of Code 2021: 20.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify problem file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Load_Mapping(Text_IO.Get_Line(file));

    while not Text_IO.End_Of_File(file) loop
        Eat_Blank : declare
            line : String := Text_IO.Get_Line(file);
        begin
            exit when line = "";
        end Eat_Blank;
    end loop;

    Load_Raster;
    Text_IO.Put_Line("There are " & Natural'Image(raster.Num_Set) & " set pixels");
    --Dump_Raster;
    filter_prev := filt(0);

    for I in 1..50 loop
        filter_prev := filt(Mapping(raster.Get_Neighbourhood(raster.x_min - 2, raster.y_min - 2)));
        Do_Iteration;
        raster.Set_Out_of_Bounds(filter_prev);
        Text_IO.Put_Line("Iter: " & Natural'Image(I) & " - There are " & Natural'Image(raster.Num_Set) & " set pixels");
        --Dump_Raster;
    end loop;

end Twenty_Second;
