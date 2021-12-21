package body Sparse_Raster is
    function Shift_Left(v : Neighbourhood; n : Natural) return Neighbourhood is begin
        return v * 2**n;
    end Shift_Left;

    function "<"(Left, Right : Point) return Boolean is begin
        if Left.x < Right.x then
            return True;
        elsif Left.x = Right.x then
            if Left.y < Right.y then
                return True;
            else
                return False;
            end if;
        else
            return False;
        end if;
    end "<";

    procedure Set_Out_of_Bounds(this : in out Raster; value : Boolean) is begin
        this.out_of_bounds := value;
    end Set_Out_of_Bounds;

    function Get_Pixel(this : in out Raster;
                       x, y : Pixel_Offset) return Boolean
    is
        use Sparse_Points;
        c : Cursor := this.mapping.Find((x => x, y => y));
    begin
        if x < this.x_min or x > this.x_max or y > this.y_max or y < this.y_min then
            return this.out_of_bounds;
        else
            return (if c = No_Element then false else true);
        end if;
    end Get_Pixel;

    function Get_Neighbourhood(this : in out Raster;
                               x, y : Pixel_Offset) return Neighbourhood
    is
        n : Neighbourhood := 0;
    begin
        for I in -1..1 loop
            for J in -1..1 loop
                n := Shift_Left(n, 1) or Neighbourhood(if this.Get_Pixel(x + Pixel_Offset(J), y + Pixel_Offset(I)) then 1 else 0);
            end loop;
        end loop;

        return n;
    end Get_Neighbourhood;

    procedure Update_Bounds(this : in out Raster;
                            x, y : Pixel_Offset)
    is begin
        if this.x_max < x then
            this.x_max := x;
        end if;

        if this.y_max < y then
            this.y_max := y;
        end if;

        if this.x_min > x then
            this.x_min := x;
        end if;

        if this.y_min > y then
            this.y_min := y;
        end if;
    end Update_Bounds;

    procedure Set_Pixel(this : in out Raster;
                        x, y : Pixel_Offset)
    is
        use Sparse_Points;
        c : Cursor := this.mapping.Find((x => x, y => y));
    begin
        if c = No_Element then
            this.mapping.Insert((x => x, y => y));
            Update_Bounds(this, x, y);
        end if;
    end Set_Pixel;

    procedure Clear_Pixel(this : in out Raster;
                          x, y : Pixel_Offset)
    is
        use Sparse_Points;
        c : Cursor := this.mapping.Find((x => x, y => y));
    begin
        if c /= No_Element then
            this.mapping.Delete(Position => c);
        end if;
    end Clear_Pixel;

    function Num_Set(this : in out Raster) return Natural is begin
        return Natural(this.mapping.Length);
    end Num_Set;

end Sparse_Raster;

