with Ada.Text_IO,
     Ada.Command_Line,
     Ada.Strings,
     Ada.Containers.Generic_Array_Sort;

procedure Third_Second is
    package Text_IO renames Ada.Text_IO;
    package Command_Line renames Ada.Command_Line;

    file : Text_IO.File_Type;

    Register_Length : constant Natural := 12;
    Report_Length : constant Natural := 1000;

    type Register is mod 2**Register_Length;
    gamma : Register := 0;

    type Report_Array is Array(Natural range <>) of Register;
    subtype Report is Report_Array(1..Report_Length);
    raw_report : Report;

    total : Natural := Report'First; -- FIXME

    oxygen, co2 : Natural;

    procedure Report_Sort is new Ada.Containers.Generic_Array_Sort(Natural, Register, Report_Array);

	function Shift_Left (Value : Register; Amount : Natural) return Register is begin
        return Value * 2 ** Amount;
    end Shift_Left;

    function Read_Value(raw : in String) return Register is
        temp : Register := 0;
    begin
        for I in 1..Register_Length loop
            temp := Shift_Left(temp, 1);

            if raw(I) = '1' then
                temp := temp or 1;
            end if;
        end loop;

        return temp;
    end Read_Value;

    -- Iteratively find the most common element until there is only one remaining
    function Find_Common(report_raw : in Report_Array;
                         most_common : in Boolean) return Register is
        start_idx : Natural := report_raw'First;
        end_idx : Natural := report_raw'Last;

        ones : Natural := 0;
        mask : Register := 0;
        zeroes : Natural := 0;
    begin

        for I in 1..Register_Length loop
            Text_IO.Put_Line("Start Index: " & Natural'Image(start_idx) & " End Index: "
                             & Natural'Image(end_idx));
            zeroes:= 0;
            mask := Shift_Left(1, Register_Length - I);

            -- Naive; walk the array
            for J in start_idx..end_idx loop
                if (mask and report_raw(J)) = 0 then
                    zeroes := zeroes + 1;
                else
                    -- There are only ones beyond this point
                    exit;
                end if;
            end loop;

            ones := ((end_idx - start_idx) + 1) - zeroes;
            Text_IO.Put_Line("  There are " & Natural'Image(ones) & " ones, and "
                             & Natural'Image(zeroes) & " zeroes");

            if most_common then
                if zeroes > ones then
                    end_idx := end_idx - ones;
                else
                    start_idx := start_idx + zeroes;
                end if;
            else
                if zeroes <= ones then
                    end_idx := end_idx - ones;
                else
                    start_idx := start_idx + zeroes;
                end if;
            end if;

            if end_idx = start_idx then
                return report_raw(start_idx);
            end if;
        end loop;

        raise Ada.Strings.Index_Error;
    end Find_Common;

begin

    Text_IO.Put_Line("Advent of Code 2021: 3.1");

    if Command_Line.Argument_Count /= 1 then
        Text_IO.Put_Line("Need to specify file containing the report. Aborting.");
        return;
    end if;

    Text_IO.Put_Line("File name: " & Command_Line.Argument(1));

    Text_IO.Open(File => file,
                 Mode => Text_IO.In_File,
                 Name => Command_Line.Argument(1));

    while not Text_IO.End_Of_File(file) loop
        raw_report(total) := Read_Value(Text_IO.Get_Line(file));
        total := total + 1;
    end loop;

    -- Sort the array of report values
    Report_Sort(raw_report);

    oxygen := Natural(Find_Common(raw_report, True));
    co2 := Natural(Find_Common(raw_report, False));

    Text_IO.Put_Line("Most common: " & Natural'Image(oxygen));
    Text_IO.Put_Line("Least common: " & Natural'Image(co2));
    Text_IO.Put_Line("Result: " & Natural'Image(oxygen * co2));

    Text_IO.Put_Line("Total Lines: " & Natural'Image(total));

end Third_Second;
