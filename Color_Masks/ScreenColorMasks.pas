// ColorMasks
//
// efg, 3 Sept 2002; 23 Feb 2003
// www.efg2.com/Lab
//
// Also see:
//
//  PNG Graphics with Delphi and Kylix
//  www.psychology.nottingham.ac.uk/staff/cr1/png.html

unit ScreenColorMasks;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls,
  Qt;     // For Png, Jpeg support using CLX
          // See www.psychology.nottingham.ac.uk/staff/cr1/png.html

type
  TFormColorMasks = class(TForm)
    ButtonRead: TButton;
    PanelImage: TPanel;
    Image: TImage;
    OpenDialog: TOpenDialog;
    ScrollBarHorizontal: TScrollBar;
    ScrollBarVertical: TScrollBar;
    LabelXYUpperLeft: TLabel;
    LabelFileName: TLabel;
    LabelResolution: TLabel;
    LabelXYLowerRight: TLabel;
    CheckBoxStretch: TCheckBox;
    GroupBoxRed: TGroupBox;
    CheckBoxR128: TCheckBox;
    CheckBoxR64: TCheckBox;
    CheckBoxR32: TCheckBox;
    CheckBoxR16: TCheckBox;
    CheckBoxR8: TCheckBox;
    CheckBoxR4: TCheckBox;
    CheckBoxR2: TCheckBox;
    CheckBoxR1: TCheckBox;
    LabelHexR: TLabel;
    LabelDecR: TLabel;
    PanelPowersOfTwo: TPanel;
    Label128: TLabel;
    Label64: TLabel;
    Label32: TLabel;
    Label16: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    Label2: TLabel;
    LabelOne: TLabel;
    GroupBoxGreen: TGroupBox;
    CheckBoxG128: TCheckBox;
    CheckBoxG64: TCheckBox;
    CheckBoxG32: TCheckBox;
    CheckBoxG16: TCheckBox;
    CheckBoxG8: TCheckBox;
    CheckBoxG4: TCheckBox;
    CheckBoxG2: TCheckBox;
    CheckBoxG1: TCheckBox;
    LabelHexG: TLabel;
    LabelDecG: TLabel;
    GroupBox2: TGroupBox;
    CheckBoxB128: TCheckBox;
    CheckBoxB64: TCheckBox;
    CheckBoxB32: TCheckBox;
    CheckBoxB16: TCheckBox;
    CheckBoxB8: TCheckBox;
    CheckBoxB4: TCheckBox;
    CheckBoxB2: TCheckBox;
    CheckBoxB1: TCheckBox;
    LabelHexB: TLabel;
    LabelDecB: TLabel;
    LabelDec: TLabel;
    LabelHex: TLabel;
    LabelCombinations: TLabel;
    LabelComboR: TLabel;
    LabelComboG: TLabel;
    LabelComboB: TLabel;
    LabelComboRGB: TLabel;
    CheckBoxContrast: TCheckBox;
    PanelCoordinates: TPanel;
    LabelXY: TLabel;
    LabelOriginal: TLabel;
    LabelMasked: TLabel;
    LabelRGBTriples: TLabel;
    ButtonAllOn: TButton;
    ButtonAllOff: TButton;
    ButtonWrite: TButton;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
    procedure ScrollBarHorizontalChange(Sender: TObject);
    procedure ScrollBarVerticalChange(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBoxStretchClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CheckBoxRGBClick(Sender: TObject);
    procedure ButtonAllOnClick(Sender: TObject);
    procedure ButtonAllOffClick(Sender: TObject);
    procedure ButtonWriteClick(Sender: TObject);
  private
     Dragging        :  BOOLEAN;

     BaseBitmap      :  TBitmap;
     BaseHeight      :  INTEGER;
     BaseWidth       :  INTEGER;

     MaskedBitmap    :  TBitmap;
     MaskBlue        :  BYTE;
     MaskGreen       :  BYTE;
     MaskRed         :  BYTE;

     ProcessCheck    :  BOOLEAN;

     StartPressPoint :  TPoint;
     StartBitmapPoint:  TPoint;

     PROCEDURE MaskAndDisplayBitmap;
     PROCEDURE SetAllFlags (CONST state:  BOOLEAN);
     PROCEDURE UpdateCorners;
     PROCEDURE UpdateBitmapSetup;

  public

  end;

var
  FormColorMasks: TFormColorMasks;

implementation
{$R *.xfm}

USES
  Math;  // Max, Min, IntPower

  TYPE
    TRGBQUAD =        // pf32pixel
    PACKED RECORD
      rgbBlue : BYTE;
      rgbGreen: BYTE;
      rgbRed  : BYTE;
      rgbReserved: Byte;
    END;
    TRGBQuadArray = ARRAY[WORD] OF TRGBQuad;
    pRGBQuadArray = ^TRGBQuadArray;

  //==  CountColors  =====================================================
  // Count number of unique R-G-B triples in a pf32bit Bitmap.
  //
  // Use 2D array of TBits objects -- when (R,G) combination occurs
  // for the first time, create 256-bit array of bits in blue dimension.
  // So, overall this is a fairly sparse matrix for most pictures.
  // Tested with pictures created with a known number of colors, including
  // a specially constructed image with 1024*1024 = 1,048,576 colors.
  //
  // efg, October 1998.
  // Converted from pf24bit to pf32bit for Kylix, Sept 2002.

  FUNCTION CountColors(CONST Bitmap:  TBitmap):  INTEGER;
    VAR
      Flags:  ARRAY[BYTE, BYTE] OF TBits;
      i    :  INTEGER;
      j    :  INTEGER;
      k    :  INTEGER;
      rowIn:  pRGBQuadArray;
  BEGIN
    // Be sure bitmap is 32-bits/pixel
    ASSERT (Bitmap.PixelFormat = pf32Bit);

    // Clear 2D array of TBits objects
    FOR j := 0 TO 255 DO
      FOR i := 0 TO 255 DO
        Flags[i,j] := NIL;

    // Step through each scanline of image
    FOR j := 0 TO Bitmap.Height-1 DO
    BEGIN
      rowIn  := Bitmap.Scanline[j];
      FOR i := 0 TO Bitmap.Width-1 DO
      BEGIN
        WITH rowIn[i] DO
        BEGIN

          IF   NOT Assigned(Flags[rgbRed, rgbGreen])
          THEN BEGIN
            // Create 3D column when needed
            Flags[rgbRed, rgbGreen] := TBits.Create;
            Flags[rgbRed, rgbGreen].Size := 256;
          END;

          // Mark this R-G-B triple
          Flags[rgbRed,rgbGreen].Bits[rgbBlue] := TRUE
        END
      END
    END;

    RESULT := 0;
    // Count and Free TBits objects
    FOR j := 0 TO 255 DO
    BEGIN
      FOR i := 0 TO 255 DO
      BEGIN

        IF   Assigned(Flags[i,j])
        THEN BEGIN
          FOR k := 0 TO 255 DO
            IF   Flags[i,j].Bits[k]
            THEN INC(RESULT);
          Flags[i,j].Free;
        END

      END
    END

  END {CountColors};


  // Create TBitmap from any format supported by TGraphic.
  FUNCTION LoadGraphicsFile(CONST Filename: STRING): TBitmap;
    VAR
      Picture:  TPicture;
  BEGIN
    Result := NIL;

    IF   FileExists(Filename)
    THEN BEGIN
      Result := TBitmap.Create;
      TRY
        Picture := TPicture.Create;
        TRY
          Picture.LoadFromFile(Filename);
          // Try converting image to bitmap
          TRY
            Result.Assign(Picture.Graphic);
          EXCEPT
            // Picture didn't support conversion to TBitmap.
            // Try drawing image on bitmap instead.
            Result.Width  := Picture.Graphic.Width;
            Result.Height := Picture.Graphic.Height;
            Result.PixelFormat := pf32bit;
            Result.Canvas.Draw(0, 0, Picture.Graphic);
          END;
        FINALLY
          Picture.Free;
        END;
      EXCEPT
        Result.Free;
        RAISE;
      END

    END

  END {LoadGraphicsFile};


PROCEDURE TFormColorMasks.MaskAndDisplayBitmap;
  VAR
    CountBlue :  BYTE;
    CountGreen:  BYTE;
    CountRed  :  BYTE;

    i         :  INTEGER;
    j         :  INTEGER;

    MaxB      :  BYTE;
    MaxG      :  BYTE;
    MaxR      :  BYTE;

    MinB      :  BYTE;
    MinG      :  BYTE;
    MinR      :  BYTE;

    RGBCount  :  INTEGER;

    RowIn     :  pRGBQuadArray;
    RowOut    :  pRGBQuadArray;

  // Brute force approach
  PROCEDURE CalcStats(CONST M128, M64, M32, M16, M8, M4, M2, M1: Boolean;
                      VAR Mask:  BYTE; VAR Count:  BYTE);
  BEGIN
    Count := 0;
    Mask := 0;

    IF   M128
    THEN BEGIN
      Mask := Mask + 128;
      INC(Count)
    END;

    IF   M64
    THEN BEGIN
      Mask := Mask +  64;
      INC(Count)
    END;

    IF   M32
    THEN BEGIN
      Mask := Mask +  32;
      INC(Count)
    END;

    IF   M16
    THEN BEGIN
      Mask := Mask +  16;
      INC(Count)
    END;

    IF   M8
    THEN BEGIN
      Mask := Mask +   8;
      INC(Count)
    END;

    IF   M4
    THEN BEGIN
      Mask := Mask +   4;
      INC(Count);
    END;

    IF   M2
    THEN BEGIN
      Mask := Mask +   2;
      INC(Count);
    END;

    IF   M1
    THEN BEGIN
      Mask := Mask +   1;
      INC(Count)
    END
  END {CalcStats};
  

  FUNCTION AdjustColor(CONST value:  BYTE; minValue, maxValue:  BYTE):  BYTE;
  BEGIN
    RESULT := value;
    IF   value > 0
    THEN BEGIN
      IF   minValue = maxValue
      THEN RESULT := 255
      ELSE RESULT := 255 * (Integer(value)    - Integer(minValue)) DIV
                           (Integer(maxValue) - Integer(minValue));
    END
  END {AdjustColor};

BEGIN
  Screen.Cursor := crHourGlass;
  TRY

    CalcStats(CheckBoxR128.Checked, CheckBoxR64.Checked,
              CheckBoxR32.Checked,  CheckBoxR16.Checked,
              CheckBoxR8.Checked,   CheckBoxR4.Checked,
              CheckBoxR2.Checked,   CheckBoxR1.Checked, MaskRed, CountRed);
    CalcStats(CheckBoxG128.Checked, CheckBoxG64.Checked,
              CheckBoxG32.Checked,  CheckBoxG16.Checked,
              CheckBoxG8.Checked,   CheckBoxG4.Checked,
              CheckBoxG2.Checked,   CheckBoxG1.Checked, MaskGreen, CountGreen);
    CalcStats(CheckBoxB128.Checked, CheckBoxB64.Checked,
              CheckBoxB32.Checked,  CheckBoxB16.Checked,
              CheckBoxB8.Checked,   CheckBoxB4.Checked,
              CheckBoxB2.Checked,   CheckBoxB1.Checked, MaskBlue, CountBlue);

    LabelDecR.Caption := IntToStr(MaskRed);
    LabelDecG.Caption := IntToStr(MaskGreen);
    LabelDecB.Caption := IntToStr(MaskBlue);

    LabelHexR.Caption := IntToHex(MaskRed,  2);
    LabelHexG.Caption := IntToHex(MaskGreen,2);
    LabelHexB.Caption := IntToHex(MaskBlue, 2);

    LabelComboR.Caption := IntToStr(Round(IntPower(2, CountRed)));
    LabelComboG.Caption := IntToStr(Round(IntPower(2, CountGreen)));
    LabelComboB.Caption := IntToStr(Round(IntPower(2, CountBlue)));

    LabelComboRGB.Caption := FormatFloat(',#',
                                IntPower(2, CountRed)  *
                                IntPower(2, CountGreen)*
                                IntPower(2, CountBlue)  );

    IF   Assigned(MaskedBitmap)
    THEN MaskedBitmap.Free;

    MaskedBitmap := TBitmap.Create;
    MaskedBitmap.PixelFormat := pf32bit;
    MaskedBitmap.Width  := BaseBitmap.Width;
    MaskedBitmap.Height := BaseBitmap.Height;

    minR := 255;
    minG := 255;
    minB := 255;

    maxR := 0;
    maxG := 0;
    maxB := 0;

    FOR j := 0 TO BaseBitmap.Height - 1 DO
    BEGIN
      RowIn  := BaseBitmap.ScanLine[j];
      RowOut := MaskedBitmap.Scanline[j];

      FOR i := 0 TO BaseBitmap.Width - 1 DO
      BEGIN
        // apply each color mask
        RowOut[i].rgbRed   := RowIn[i].rgbRed   AND MaskRed;
        RowOut[i].rgbGreen := RowIn[i].rgbGreen AND MaskGreen;
        RowOut[i].rgbBlue  := RowIn[i].rgbBlue  AND MaskBlue;

        IF   CheckBoxContrast.Checked
        THEN BEGIN
          // keep min / max info before applying mask
          IF   RowOut[i].rgbRed > maxR
          THEN maxR := RowOut[i].rgbRed
          ELSE BEGIN
            IF   RowOut[i].rgbRed < minR
            THEN minR := RowOut[i].rgbRed;
          END;

          IF   RowOut[i].rgbGreen > maxG
          THEN maxG := RowOut[i].rgbGreen
          ELSE BEGIN
            IF   RowOut[i].rgbGreen < minG
            THEN minG := RowOut[i].rgbGreen;
          END;

          IF   RowOut[i].rgbBlue > maxB
          THEN maxB := RowOut[i].rgbBlue
          ELSE BEGIN
            IF   RowOut[i].rgbBlue < minB
            THEN minB := RowOut[i].rgbBlue;
          END;

        END;
      END
    END;

    RGBCount := CountColors(MaskedBitmap);
    LabelRGBTriples.Caption := 'RGB Triples = ' + FormatFloat(',#', RGBCount);

    IF   CheckBoxContrast.Checked
    THEN BEGIN

      FOR j := 0 TO MaskedBitmap.Height - 1 DO
      BEGIN
        RowIn  := BaseBitmap.ScanLine[j];
        RowOut := MaskedBitmap.Scanline[j];

        FOR i := 0 TO MaskedBitmap.Width - 1 DO
        BEGIN
          RowOut[i].rgbRed   := AdjustColor(RowIn[i].rgbRed   AND MaskRed,   minR, maxR);
          RowOut[i].rgbGreen := AdjustColor(RowIn[i].rgbGreen AND MaskGreen, minG, maxG);
          RowOut[i].rgbBlue  := AdjustColor(RowIn[i].rgbBlue  AND MaskBlue,  minB, maxB);
        END
      END
    END;

    Image.Picture.Graphic := MaskedBitmap

  FINALLY
    Screen.Cursor := crDefault
  END
END {MaskAndDisplayBitmap};


PROCEDURE TFormColorMasks.SetAllFlags (CONST state:  BOOLEAN);
BEGIN
  ProcessCheck := FALSE;
  CheckBoxR128.Checked := state;
  CheckBoxR64.Checked := state;
  CheckBoxR32.Checked := state;
  CheckBoxR16.Checked := state;

  CheckBoxR8.Checked := state;
  CheckBoxR4.Checked := state;
  CheckBoxR2.Checked := state;
  CheckBoxR1.Checked := state;

  CheckBoxG128.Checked := state;
  CheckBoxG64.Checked := state;
  CheckBoxG32.Checked := state;
  CheckBoxG16.Checked := state;

  CheckBoxG8.Checked := state;
  CheckBoxG4.Checked := state;
  CheckBoxG2.Checked := state;
  CheckBoxG1.Checked := state;

  CheckBoxB128.Checked := state;
  CheckBoxB64.Checked := state;
  CheckBoxB32.Checked := state;
  CheckBoxB16.Checked := state;

  CheckBoxB8.Checked := state;
  CheckBoxB4.Checked := state;
  CheckBoxB2.Checked := state;
  CheckBoxB1.Checked := state;

  ProcessCheck := TRUE;

  // If all checks TRUE, no need for contrast enhancement;
  // If all checks FALSE, contrast enhancement likely needed to see anything.
  CheckBoxContrast.Checked := not STATE;

END {SetAllFlags};


PROCEDURE TFormColorMasks.UpdateCorners;
BEGIN
  IF   CheckBoxStretch.Checked
  THEN BEGIN
    LabelXYUpperLeft.Caption := 'UpperLeft = (0, 0)';

    LabelXYLowerRight.Caption :=
      'LowerRight = (' +
      IntToStr(BaseBitmap.Width  - 1) +
      ', ' +
      IntToStr(BaseBitmap.Height - 1) +
      ')'

  END
  ELSE BEGIN
    LabelXYUpperLeft.Caption :=
      'UpperLeft = (' +
      IntToStr(ScrollBarHorizontal.Position) +
      ', ' +
      IntToStr(ScrollBarVertical.Position) +
      ')';

    LabelXYLowerRight.Caption :=
      'LowerRight = (' +
      IntToStr(ScrollBarHorizontal.Position + PanelImage.Width  - 1) +
      ', ' +
      IntToStr(ScrollBarVertical.Position   + PanelImage.Height - 1) +
      ')'
  END
END {UpdateCorners};


PROCEDURE TFormColorMasks.UpdateBitmapSetup;
BEGIN
  LabelResolution.Caption := 'Resolution = ' +
    IntToStr(BaseBitmap.Width) + '-by-' +
    IntToStr(BaseBitmap.Height) + ' pixels';

  IF   CheckBoxStretch.Checked
  THEN BEGIN
    PanelImage.Width  := BaseWidth;   // use default panel size
    PanelImage.Height := BaseHeight;

    Image.Width  := PanelImage.Width;
    Image.Height := PanelImage.Height;
    Image.Left := 0;
    Image.Top := 0;
    ScrollBarHorizontal.Visible := FALSE;
    ScrollBarVertical.Visible   := FALSE;
  END
  ELSE BEGIN
    PanelImage.Width := Min(BaseBitmap.Width, BaseWidth);     // avoid flicker
    Image.Width := BaseBitmap.Width;
    ScrollBarHorizontal.Max := Max(Image.Width - PanelImage.Width, 0);
    ScrollBarHorizontal.Position := 0;
    ScrollBarHorizontal.Visible := (BaseBitmap.Width > PanelImage.Width);

    PanelImage.Height := Min(BaseBitmap.Height, BaseHeight);  // avoid flicker
    Image.Height := BaseBitmap.Height;
    ScrollBarVertical.Max :=  Max(Image.Height - PanelImage.Height, 0);
    ScrollBarVertical.Position := 0;
    ScrollBarVertical.Visible :=  (BaseBitmap.Height > PanelImage.Height)
  END;

  UpdateCorners;
  LabelXY.Caption  := '';
  LabelOriginal.Caption := '';
  LabelMasked.Caption := '';
END {UpdateBitmapSetup};


procedure TFormColorMasks.FormCreate(Sender: TObject);
begin
  WITH FormColorMasks.Constraints DO
  BEGIN
    MaxHeight := Height;
    MaxWidth  := Width;
    MinHeight := Height;
    MinWidth  := Width
  END;

  // Usenet posts say this works, but not for me.
  TPicture.RegisterFileformat('jpg', 'JPEG file', TBitmap);

  BaseWidth  := PanelImage.Width;   // save default panel sizes
  BaseHeight := PanelImage.Height;

  BaseBitmap := TBitmap.Create;
  BaseBitmap.PixelFormat := pf32bit;  // no pf24bit in CLX
  BaseBitmap.Width       := Image.Width;
  BaseBitmap.Height      := Image.Height;

  BaseBitmap.Canvas.Brush.Color := clBlue;
  BaseBitmap.Canvas.FillRect(BaseBitmap.Canvas.ClipRect);

  LabelFileName.Caption := 'File = <default file>';
  UpdateBitmapSetup;
  MaskAndDisplayBitmap;

  Dragging := FALSE;
  ProcessCheck := TRUE;

  OpenDialog.Filter := GraphicFilter(TGraphic);
end;

procedure TFormColorMasks.FormDestroy(Sender: TObject);
begin
  IF   Assigned(BaseBitmap)
  THEN BaseBitmap.Free;

  IF   Assigned(MaskedBitmap)
  THEN MaskedBitmap.Free
end;




procedure TFormColorMasks.ButtonReadClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  OpenDialog.Filter :=        // GraphicFilter(TGraphic);
    // Restrict to list of filetypes that really work
    'All(*.bmp;*.png;*.ico)|*.bmp;*.png;*.ico|' +
    'BMP file (*.bmp)|*.bmp|' +
    'PNG file (*.png)|*.png|' +
    'ICO file (*.ico)|*.ico';
{$ENDIF}

{$IFDEF LINUX}
 OpenDialog.Filter :=        // GraphicFilter(TGraphic);
    // Restrict to list of filetypes that really work
    'All(*.jpg;*.bmp;*.png;*.ico)|*.jpg;*.bmp;*.png;*.gif;*.ico|' +
    'JPG file (*.jpg)|*.jpg|' +
    'BMP file (*.bmp)|*.bmp|' +
    'PNG file (*.png)|*.png|' +
    'ICO file (*.ico)|*.ico';
{$ENDIF}

  IF   OpenDialog.Execute
  THEN BEGIN
    BaseBitmap.Free;

    BaseBitmap := LoadGraphicsFile(OpenDialog.Filename);
    IF   BaseBitmap.PixelFormat <> pf32bit    // For Kylix compatibility
    THEN BaseBitmap.PixelFormat := pf32bit;

    LabelFilename.Caption   := 'File = '       + OpenDialog.FileName;

    CheckBoxStretch.Visible := ( (BaseBitmap.Width  <> BaseWidth) OR
                                 (BaseBitmap.Height <> BaseHeight) );

    UpdateBitmapSetup;

    MaskAndDisplayBitmap
  END
end;

procedure TFormColorMasks.ScrollBarHorizontalChange(Sender: TObject);
begin
  Image.Left := -ScrollBarHorizontal.Position;
  UpdateCorners
end;

procedure TFormColorMasks.ScrollBarVerticalChange(Sender: TObject);
begin
  Image.Top := -ScrollBarVertical.Position;
  UpdateCorners
end;

procedure TFormColorMasks.ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Dragging := TRUE;

  // Dragging start point
  StartPressPoint  := Point(X,Y);

  // Location of image withing panel
  StartBitmapPoint := Point(Image.Left, Image.Top)
end;


procedure TFormColorMasks.ImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

  VAR
    RGB    :  TRGBQuad;
    i      :  INTEGER;
    j      :  INTEGER;
    xDelta :  INTEGER;
    yDelta :  INTEGER;
begin
  IF   Dragging
  THEN BEGIN
    // If image is smaller than panel and scroll bars are not present,
    // don't bother dragging image around.
    IF   ScrollBarHorizontal.Visible
    THEN BEGIN
      xDelta := X - StartPressPoint.X;

      // Adjust location of upper left corner of image from
      // dragging image.
      Image.Left :=
        Max(-Image.Width + PanelImage.Width,
          Min(0,
            StartBitmapPoint.X + xDelta));

      // Update scrollbar
      ScrollBarHorizontal.Position := -Image.Left;

      // Because Image.Left point changed, the location of the
      // image within the panel must also be updated.
      StartBitmapPoint.X := Image.Left
    END;

    IF   ScrollBarVertical.Visible
    THEN BEGIN
      yDelta := Y - StartPressPoint.Y;

      Image.Top  :=
        Max(-Image.Height + PanelImage.Height,
          Min(0,
            StartBitmapPoint.Y + yDelta));

      ScrollBarVertical.Position   := -Image.Top;
      StartBitmapPoint.Y := Image.Top
    END;

    UpdateCorners
  END;

  IF   CheckBoxStretch.Checked
  THEN BEGIN
    i := Round(X * BaseBitmap.Width  / PanelImage.Width);
    j := Round(Y * BaseBitmap.Height / PanelImage.Height);

    LabelXY.Caption := '(X, Y) = (' + IntToStr(i) + ', ' + IntToStr(j) + ')'
  END
  ELSE BEGIN
    i := X;
    j := Y;
    LabelXY.Caption := '(X, Y) = (' +  IntToStr(i) + ', ' + IntToStr(j) + ')';
  END;

  RGB := pRGBQuadArray(BaseBitmap.ScanLine[j])[i];
  LabelOriginal.Caption := 'Original(R, G, B) = Decimal(' +
                      IntToStr(RGB.rgbRed) + ', ' +
                      IntToStr(RGB.rgbGreen) + ', ' +
                      IntToStr(RGB.rgbBlue) + ') = Hex(' +
                      IntToHex(RGB.rgbRed,2) + ', ' +
                      IntToHex(RGB.rgbGreen,2) + ', ' +
                      IntToHex(RGB.rgbBlue,2) + ')';

  RGB := pRGBQuadArray(BaseBitmap.ScanLine[j])[i];
  RGB.rgbRed   := rGB.rgbRed   AND MaskRed;
  RGB.rgbGreen := rGB.rgbGreen AND MaskGreen;
  RGB.rgbBlue  := rGB.rgbBlue  AND MaskBlue;
  LabelMasked.Caption := 'Masked(R, G, B) = Decimal(' +
                      IntToStr(RGB.rgbRed) + ', ' +
                      IntToStr(RGB.rgbGreen) + ', ' +
                      IntToStr(RGB.rgbBlue) + ') = Hex(' +
                      IntToHex(RGB.rgbRed,2) + ', ' +
                      IntToHex(RGB.rgbGreen,2) + ', ' +
                      IntToHex(RGB.rgbBlue,2) + ')';
end;


procedure TFormColorMasks.ImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Dragging := FALSE
end;

procedure TFormColorMasks.CheckBoxStretchClick(Sender: TObject);
begin
  Image.Stretch := CheckBoxStretch.Checked;
  UpdateBitmapSetup
end;

procedure TFormColorMasks.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  LabelXY.Caption  := '';
  LabelOriginal.Caption := '';
  LabelMasked.Caption := '';
end;

procedure TFormColorMasks.CheckBoxRGBClick(Sender: TObject);
begin
  IF   ProcessCheck
  THEN MaskAndDisplayBitmap;
end;

procedure TFormColorMasks.ButtonAllOnClick(Sender: TObject);
begin
  SetAllFlags(TRUE)
end;

procedure TFormColorMasks.ButtonAllOffClick(Sender: TObject);
begin
  SetAllFlags(FALSE)
end;


// See:  PNG Graphics with Delphi and Kylix
// www.psychology.nottingham.ac.uk/staff/cr1/png.html
// and suggestion by Eric Sibert in 14 Nov 2001 posting
// to borland.public.delphi.graphics

procedure TFormColorMasks.ButtonWriteClick(Sender: TObject);
  var
   lWideStr:  WideString;
begin
{$IFDEF MSWINDOWS}
  SaveDialog.Filter :=        // GraphicFilter(TGraphic);
    // Restrict to list of filetypes that really work
    'All(*.bmp;*.png)|*.bmp;*.png|' +
    'BMP file (*.bmp)|*.bmp|' +
    'PNG file (*.png)|*.png';
{$ENDIF}

{$IFDEF LINUX}
 SaveDialog.Filter :=        // GraphicFilter(TGraphic);
    // Restrict to list of filetypes that really work
    'All(*.jpg;*.bmp;*.png)|*.jpg;*.bmp;*.png|' +
    'JPG file (*.jpg)|*.jpg|' +
    'BMP file (*.bmp)|*.bmp|' +
    'PNG file (*.png)|*.png';
{$ENDIF}

  IF   SaveDialog.Execute
  THEN BEGIN
    IF   UpperCase(ExtractFileExt(SaveDialog.FileName)) = '.PNG'
    THEN BEGIN
      lWideStr := SaveDialog.Filename;
      QPixMap_save (MaskedBitmap.Handle, @lWideStr, pChar('PNG'));
    END;

    IF   UpperCase(ExtractFileExt(SaveDialog.FileName)) = '.JPG'
    THEN BEGIN
      lWideStr := SaveDialog.Filename;
      QPixMap_save(MaskedBitmap.Handle, @lWideStr, pChar('JPEG'), 80 {quality});
    END;

   IF   UpperCase(ExtractFileExt(SaveDialog.FileName)) = '.BMP'
   THEN MaskedBitmap.SaveToFile(SaveDialog.Filename)

  END
end;

end.
