with Ada.Containers.Ordered_Sets;

package Sparse_Raster is
    type Neighbourhood is mod 2**9;
    type Pixel_Offset is new Integer;
    subtype Raster_Bound is Pixel_Offset;

    type Point is record
        x, y : Pixel_Offset;
    end record;

    function "<"(Left, Right : Point) return Boolean;
    package Sparse_Points is new Ada.Containers.Ordered_Sets(
        Element_Type => Point);

    type Raster is tagged record
        x_max, y_max : Raster_Bound := Raster_Bound'First;
        x_min, y_min : Raster_Bound := Raster_Bound'Last;
        mapping : Sparse_Points.Set;

        out_of_bounds : Boolean := False;
    end record;
    procedure Set_Out_of_Bounds(this : in out Raster; value : Boolean);
    function Get_Pixel(this : in out Raster;
                       x, y : Pixel_Offset) return Boolean;
    function Get_Neighbourhood(this : in out Raster;
                               x, y : Pixel_Offset) return Neighbourhood;
    procedure Set_Pixel(this : in out Raster;
                        x, y : Pixel_Offset);
    procedure Clear_Pixel(this : in out Raster;
                          x, y : Pixel_Offset);
    function Num_Set(this : in out Raster) return Natural;
    procedure Update_Bounds(this : in out Raster;
                            x, y : Pixel_Offset);

private

    function Shift_Left(v : Neighbourhood; n : Natural) return Neighbourhood;

end Sparse_Raster;

