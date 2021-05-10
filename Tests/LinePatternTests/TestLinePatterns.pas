unit TestLinePatterns;

interface

uses
  TestFrameWork, GR32;

{$DEFINE RUN_BENCHMARKS}

type
  TTestLinePatterns = class(TTestCase)
  published
    procedure LineDot_HorizontalLeftToRight;
    procedure LineDot_HorizontalLeftToRight_WithoutLastPixel;
    procedure LineDot_HorizontalRightToLeft;
    procedure LineDot_HorizontalRightToLeft_WithoutLastPixel;
    procedure LineDot_VerticalTopToBottom;
    procedure LineDot_VerticalTopToBottom_WithoutLastPixel;
    procedure LineDot_VerticalBottomToTop;
    procedure LineDot_VerticalBottomToTop_WithoutLastPixel;
    procedure LineDot_DiagonalRightDown;
    procedure LineDot_SlightlyRightDown;
    procedure LineDot_WorksInAllDirections;
    procedure LineDot_InMeasuringModeDrawsNothing;
    procedure LineDot_InMeasuringModeUpdatesLine;

{$IFDEF RUN_BENCHMARKS} published {$ELSE} private {$ENDIF}
    procedure Line_Benchmark;
    procedure LineDot_Benchmark;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    Have, Want: TBitmap32;
    ChangeCount: Integer;
    ChangeArea: TRect;
    ChangeInfo: Cardinal;
    procedure RememberLastChangeEvent(Sender: TObject; const Area: TRect;
      const Info: Cardinal);
  end;

implementation

uses
  Bitmap32CompareDialogUnit, System.Classes, System.Diagnostics, System.SysUtils,
  System.Types, GR32_Paths, GR32_Brushes, GR32_Polygons;

procedure TTestLinePatterns.SetUp;
begin
  Have := TBitmap32.Create;
  Want := TBitmap32.Create;
end;

procedure TTestLinePatterns.TearDown;
begin
  Have.Free;
  Want.Free;
end;

procedure TTestLinePatterns.LineDot_HorizontalLeftToRight;
var
  I, Y: Integer;
begin
  Want.LoadFromFile('HorizontalDotLinesLeftToRight.bmp');
  Have.SetSize(22, 41);
  for I := 0 to 19 do
  begin
    Y := 1 + 2 * I;
    Have.LineDot(1, Y, 1 + I, Y, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_HorizontalLeftToRight_WithoutLastPixel;
var
  I, Y: Integer;
begin
  Want.LoadFromFile('HorizontalDotLinesLeftToRight.bmp');
  Have.SetSize(22, 41);
  for I := 1 to 20 do
  begin
    Y := 1 + 2 * (I - 1);
    Have.LineDot(1, Y, 1 + I, Y, clAqua32, false);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_HorizontalRightToLeft;
var
  I, Y: Integer;
begin
  Want.LoadFromFile('HorizontalDotLinesRightToLeft.bmp');
  Have.SetSize(22, 41);
  for I := 0 to 19 do
  begin
    Y := 1 + 2 * I;
    Have.LineDot(20, Y, 20 - I, Y, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_HorizontalRightToLeft_WithoutLastPixel;
var
  I, Y: Integer;
begin
  Want.LoadFromFile('HorizontalDotLinesRightToLeft.bmp');
  Have.SetSize(22, 41);
  for I := 1 to 20 do
  begin
    Y := 1 + 2 * (I - 1);
    Have.LineDot(20, Y, 20 - I, Y, clAqua32, false);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_VerticalTopToBottom;
var
  I, X: Integer;
begin
  Want.LoadFromFile('VerticalDotLinesTopToBottom.bmp');
  Have.SetSize(41, 22);
  for I := 0 to 19 do
  begin
    X := 1 + 2 * I;
    Have.LineDot(X, 1, X, 1 + I, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_VerticalTopToBottom_WithoutLastPixel;
var
  I, X: Integer;
begin
  Want.LoadFromFile('VerticalDotLinesTopToBottom.bmp');
  Have.SetSize(41, 22);
  for I := 1 to 20 do
  begin
    X := 1 + 2 * (I - 1);
    Have.LineDot(X, 1, X, 1 + I, clAqua32, false);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_VerticalBottomToTop;
var
  I, X: Integer;
begin
  Want.LoadFromFile('VerticalDotLinesBottomToTop.bmp');
  Have.SetSize(41, 22);
  for I := 0 to 19 do
  begin
    X := 1 + 2 * I;
    Have.LineDot(X, 20, X, 20 - I, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_VerticalBottomToTop_WithoutLastPixel;
var
  I, X: Integer;
begin
  Want.LoadFromFile('VerticalDotLinesBottomToTop.bmp');
  Have.SetSize(41, 22);
  for I := 1 to 20 do
  begin
    X := 1 + 2 * (I - 1);
    Have.LineDot(X, 20, X, 20 - I, clAqua32, false);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_DiagonalRightDown;
var
  I, X: Integer;
begin
  Want.LoadFromFile('DiagonalDotLinesRightDown.bmp');
  Have.SetSize(55, 16);
  for I := 0 to 13 do
  begin
    X := 1 + 3 * I;
    Have.LineDot(X, 1, X + I, 1 + I, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_SlightlyRightDown;
var
  I, Y: Integer;
begin
  Want.LoadFromFile('DotLinesSlightlyRightDown.bmp');
  Have.SetSize(19, 52);
  for I := 0 to 16 do
  begin
    Y := 1 + 3 * I;
    Have.Line(1, Y, 1 + I, Y + 1, $FF800000, true);
    Have.LineDot(1, Y, 1 + I, Y + 1, clAqua32, true);
  end;
  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_WorksInAllDirections;
begin
  Want.LoadFromFile('DotLinesInAllDirections.bmp');
  Have.SetSize(51, 51);

  Have.Line(0, 0, 25, 25, clLtGray32, false);
  Have.Line(20, 0, 25, 25, clLtGray32, false);
  Have.Line(30, 0, 25, 25, clLtGray32, false);
  Have.Line(50, 0, 25, 25, clLtGray32, false);
  Have.Line(50, 20, 25, 25, clLtGray32, false);
  Have.Line(50, 30, 25, 25, clLtGray32, false);
  Have.Line(50, 50, 25, 25, clLtGray32, false);
  Have.Line(30, 50, 25, 25, clLtGray32, false);
  Have.Line(20, 50, 25, 25, clLtGray32, false);
  Have.Line(0, 50, 25, 25, clLtGray32, false);
  Have.Line(0, 30, 25, 25, clLtGray32, false);
  Have.Line(0, 20, 25, 25, clLtGray32, false);

  Have.LineDot(0, 0, 25, 25, clRed32, false);
  Have.LineDot(20, 0, 25, 25, clRed32, false);
  Have.LineDot(30, 0, 25, 25, clRed32, false);
  Have.LineDot(50, 0, 25, 25, clRed32, false);
  Have.LineDot(50, 20, 25, 25, clRed32, false);
  Have.LineDot(50, 30, 25, 25, clRed32, false);
  Have.LineDot(50, 50, 25, 25, clRed32, false);
  Have.LineDot(30, 50, 25, 25, clRed32, false);
  Have.LineDot(20, 50, 25, 25, clRed32, false);
  Have.LineDot(0, 50, 25, 25, clRed32, false);
  Have.LineDot(0, 30, 25, 25, clRed32, false);
  Have.LineDot(0, 20, 25, 25, clRed32, false);

  Have.LineDot(25, 25, 0, 0, clGreen32, false);
  Have.LineDot(25, 25, 20, 0, clGreen32, false);
  Have.LineDot(25, 25, 30, 0, clGreen32, false);
  Have.LineDot(25, 25, 50, 0, clGreen32, false);
  Have.LineDot(25, 25, 50, 20, clGreen32, false);
  Have.LineDot(25, 25, 50, 30, clGreen32, false);
  Have.LineDot(25, 25, 50, 50, clGreen32, false);
  Have.LineDot(25, 25, 30, 50, clGreen32, false);
  Have.LineDot(25, 25, 20, 50, clGreen32, false);
  Have.LineDot(25, 25, 0, 50, clGreen32, false);
  Have.LineDot(25, 25, 0, 30, clGreen32, false);
  Have.LineDot(25, 25, 0, 20, clGreen32, false);

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_InMeasuringModeDrawsNothing;
begin
  Want.SetSize(10, 10);
  Have.SetSize(10, 10);

  Have.BeginMeasuring(nil);
  Have.LineDot(1, 1, 9, 9, clRed32);
  Have.EndMeasuring;

  CheckBitmapsEqual(Want, Have);
end;

procedure TTestLinePatterns.LineDot_InMeasuringModeUpdatesLine;
begin
  Have.SetSize(20, 20);
  Have.BeginMeasuring(RememberLastChangeEvent);
  Have.LineDot(1, 2, 15, 10, clRed32);
  Have.EndMeasuring;
  CheckEquals(1, ChangeCount);
  // TODO: the +1 is in the Line functions, I am not sure how this works. Once this is
  // clear this test can probably be more informative.
  CheckEquals(AREAINFO_LINE + 1, ChangeInfo);
  CheckEquals(1, ChangeArea.Left);
  CheckEquals(2, ChangeArea.Top);
  CheckEquals(15, ChangeArea.Right);
  CheckEquals(10, ChangeArea.Bottom);
end;

procedure TTestLinePatterns.RememberLastChangeEvent(Sender: TObject; const Area: TRect;
  const Info: Cardinal);
begin
  Inc(ChangeCount);
  ChangeArea := Area;
  ChangeInfo := Info;
end;

procedure TTestLinePatterns.Line_Benchmark;
var
  Watch: TStopwatch;
  X1, X2, Y1, Y2: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y1 := 0 to 999 do
    for Y2 := 0 to 999 do
      Have.Line(0, Y1, 999, Y2, clRed32, true);

  for X1 := 0 to 999 do
    for X2 := 0 to 999 do
      Have.Line(X1, 0, X2, 999, clRed32, true);

  Watch.Stop;
  Have.SaveToFile('Line_Benchmark.bmp');
  Fail(Format('Line took %d ms', [Watch.ElapsedMilliseconds]));
end;

procedure TTestLinePatterns.LineDot_Benchmark;
var
  Watch: TStopwatch;
  X1, X2, Y1, Y2: Integer;
begin
  Have.SetSize(1000, 1000);
  Watch := TStopwatch.StartNew;

  for Y1 := 0 to 999 do
    for Y2 := 0 to 999 do
      Have.LineDot(0, Y1, 999, Y2, clRed32, true);

  for X1 := 0 to 999 do
    for X2 := 0 to 999 do
      Have.LineDot(X1, 0, X2, 999, clRed32, true);

  Watch.Stop;
  Have.SaveToFile('LineDot_Benchmark.bmp');
  Fail(Format('LineDot took %d ms', [Watch.ElapsedMilliseconds]));
end;

initialization

TestFrameWork.RegisterTest(TTestLinePatterns.Suite);

end.
