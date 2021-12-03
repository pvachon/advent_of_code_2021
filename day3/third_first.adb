with Ada.Text_IO,
     Ada.Command_Line;

procedure Third_First is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    Register_Length : constant Natural := 12;

    type Counter is Array(1..Register_Length) of Natural;
    counts : Counter := (others => 0); -- Count number of bits that are set

    total, zeros : Natural := 0;

    type Register is mod 2**Register_Length;
    gamma : Register := 0;

	function Shift_Left (Value : Register; Amount : Natural) return Register is begin
        return Value * 2;
    end Shift_Left;

    procedure Check_Bits(raw : in String) is begin
        for I in Counter'Range loop
            if raw(I) = '1' then
                counts(I) := counts(I) + 1;
            end if;
        end loop;
    end Check_Bits;
begin

    Text_IO.Put_Line("Advent of Code 2021: 3.0");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file containing the report. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        Check_Bits(Text_IO.Get_Line(file));
        total := total + 1;
    end loop;

    for I in Counter'Range loop
        zeros := total - counts(I);

        gamma := Shift_Left(gamma, 1);
        if zeros < counts(I) then
            gamma := gamma or 1;
        end if;
    end loop;

    Text_IO.Put_Line("Total Lines: " & Natural'Image(total) & " Gamma: " & Register'Image(gamma) &
                     " Epsilon: " & Register'Image(not gamma));

    Text_IO.Put_Line("Voltage: " & Natural'Image(Natural(gamma) * Natural(not gamma)));

end Third_First;
