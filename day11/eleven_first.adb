with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Containers.Vectors,
     Ada.Strings;

procedure Eleven_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    Raster_Columns : constant Natural := 10;
    Raster_Rows : constant Natural := 10;
    Total_Steps : constant Natural := 100;

    type Octopus is range 0..10;

    type Line is array(1..Raster_Columns) of Octopus;
    type Raster is array(1..Raster_Rows) of Line;

    octopodes : Raster;

    procedure Parse_Raster_Line(raw_line : String;
                                out_line : out Line) is begin
        if raw_line'Length /= out_line'Length then
            raise Ada.Strings.Index_Error;
        end if;

        for I in out_line'Range loop
            out_line(I) := Character'Pos(raw_line(I)) - Character'Pos('0');
        end loop;
    end Parse_Raster_Line;

    function Octopus_Flash(octopodes : in out Raster;
                           ox, oy: in Natural) return Natural
    is
        type Point is record
            X, Y : Natural;
        end record;

        package Visit_Stack is new Ada.Containers.Vectors(
            Index_Type => Natural,
            Element_Type => Point);

        stack : Visit_Stack.Vector;

        procedure Push(pt : Point) is begin
            stack.Append(pt);
        end Push;

        function Pop return Point is
            pt : Point := stack(stack.Last_Index);
        begin
            stack.Delete_Last;
            return pt;
        end Pop;

        flash_count : Natural := 0;

        function Increment(X, Y : Natural) return Boolean is
            flashed : Boolean := False;
        begin
            case octopodes(Y)(X) is
                when 0 => return False;
                when 9 => flashed := True; octopodes(Y)(X) := 10;
                when 10 => return False;
                when others =>  octopodes(Y)(X) := octopodes(Y)(X) + 1;
            end case;

            return flashed;
        end Increment;
    begin
        Push((X => ox, Y => oy));

        while not stack.Is_Empty loop
            Check_Neigh : declare
                pt : Point := Pop;
            begin
                -- Check if this octopus has flashed since we added it to the queue
                if octopodes(pt.Y)(pt.X) = 0 then
                    goto Continue;
                end if;

                -- Check the neighbourhood and add them to the queue if they would
                -- be expected to flash, or have already flashed.
                if pt.X /= Line'First then
                    if Increment(pt.X - 1, pt.Y) then
                        Push((X => pt.X - 1, Y => pt.Y));
                    end if;

                    if pt.Y /= Raster'Last then
                        if Increment(pt.X - 1, pt.Y + 1) then
                            Push((X=> pt.X - 1, Y => pt.Y + 1));
                        end if;
                    end if;

                    if pt.Y /= Raster'First then
                        if Increment(pt.X - 1, pt.Y - 1) then
                            Push((X => pt.X - 1, Y => pt.Y - 1));
                        end if;
                    end if;
                end if;

                if pt.X /= Line'Last then
                    if Increment(pt.X + 1, pt.Y) then
                        Push((X => pt.X + 1, Y => pt.Y));
                    end if;

                    if pt.Y /= Raster'Last then
                        if Increment(pt.X + 1, pt.Y + 1) then
                            Push((X => pt.X + 1, Y => pt.Y + 1));
                        end if;
                    end if;

                    if pt.Y /= Raster'First then
                        if Increment(pt.X + 1, pt.Y - 1) then
                            Push((X => pt.X + 1, Y => pt.Y - 1));
                        end if;
                    end if;
                end if;

                if pt.Y /= Raster'Last then
                    if Increment(pt.X, pt.Y + 1) then
                        Push((X => pt.X, Y => pt.Y + 1));
                    end if;
                end if;

                if pt.Y /= Raster'First then
                    if Increment(pt.X, pt.Y - 1) then
                        Push((X => pt.X, Y => pt.Y - 1));
                    end if;
                end if;

                -- This octopus has flashed.
                octopodes(pt.Y)(pt.X) := 0;
            end Check_Neigh;

            flash_count := flash_count + 1;

            <<Continue>>
        end loop;

        return flash_count;
    end Octopus_Flash;

    function Do_Step(octopodes : in out Raster) return Natural is
        flash_count : Natural := 0;
    begin
        -- Increment everything
        for J in Raster'Range loop
            for I in Line'Range loop
                octopodes(J)(I) := octopodes(J)(I) + 1;
            end loop;
        end loop;

        for J in Raster'Range loop
            for I in Line'Range loop
                if octopodes(J)(I) = 10 then
                    flash_count := flash_count + Octopus_Flash(octopodes, I, J);
                end if;
            end loop;
        end loop;

        return flash_count;
    end Do_Step;

    total_flashes : Natural := 0;
begin

    Text_IO.Put_Line("Advent of Code 2021: 11.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify input file. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    Read_Raster : declare
        line_number : Natural := 0;
    begin
        while not Text_IO.End_Of_File(file) loop
            line_number := line_number + 1;
            Parse_Raster_Line(Text_IO.Get_Line(file), octopodes(line_number));
        end loop;

        if line_number /= octopodes'Length then
            raise Ada.Strings.Index_Error;
        end if;
    end Read_Raster;

    for I in 1..Total_Steps loop
        Text_IO.Put_Line("Iteration " & Natural'Image(I));
        total_flashes := total_flashes + Do_Step(octopodes);
    end loop;

    Text_IO.Put_Line("Total flashes after steps: " & Natural'Image(total_flashes));
end Eleven_First;
