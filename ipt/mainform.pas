unit MainForm;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, UIKit, Foundation, CoreGraphics;

type

  { TMainForm }

  TMainForm = objcclass (UIResponder, UIApplicationDelegateProtocol)
  private
    _window: UIWindow;
    Label1: UILabel;
    Button1: UIButton;
    ExitButton: UIButton;
  public
    function application_didFinishLaunchingWithOptions(application: UIApplication; launchOptions: NSDictionary): Boolean; message 'application:didFinishLaunchingWithOptions:';
    procedure applicationWillResignActive(application: UIApplication); message 'applicationWillResignActive:';
    procedure applicationDidEnterBackground(application: UIApplication); message 'applicationDidEnterBackground:';
    procedure applicationWillEnterForeground(application: UIApplication); message 'applicationWillEnterForeground:';
    procedure applicationDidBecomeActive(application: UIApplication); message 'applicationDidBecomeActive:';
    procedure applicationWillTerminate(application: UIApplication); message 'applicationWillTerminate:';

    procedure Button1Click(Sender: id); message 'Button1Click:';
    procedure ExitButtonClick(Sender: id); message 'ExitButtonClick:';
  end;

implementation

function NSSTR(str: string): NSString; inline;
begin
  Result := NSString.classStringWithUTF8String(PChar(str));
end;

{ TMainForm }

function TMainForm.application_didFinishLaunchingWithOptions(
  application: UIApplication; launchOptions: NSDictionary): Boolean;
begin
  _window := UIWindow.alloc.initWithFrame(UIScreen.classMainScreen.bounds);
  _window.setBackgroundColor(UIColor.classGrayColor);

  Label1 := UILabel.alloc.init;
  Label1.setFrame(CGRectMake(50,30,200,50));
  Label1.setText(NSString.classStringWithUTF8String('label1 text'));
  Label1.setBackgroundColor(UIColor.classWhiteColor);
  Label1.setTextAlignment(UITextAlignmentCenter);
  _window.addSubView(Label1);

  Button1 := UIButton.classButtonWithType(UIButtonTypeRoundedRect).retain;
  Button1.setFrame(CGRectMake(100,100,100,100));
  Button1.setTitle_forState(NSString.classStringWithUTF8String('Anton'), UIControlStateNormal);
  Button1.setBackgroundColor(UIColor.classGreenColor);
  Button1.addTarget_action_forControlevents(self, objcselector('Button1Click:'), UIControlEventTouchUpInside);
  Button1.setTintColor(UIColor.classRedColor);
  _window.addSubview(Button1);

  ExitButton := UIButton.classButtonWithType(UIButtonTypeRoundedRect).retain;
  ExitButton.setFrame(CGRectMake(100,400,100,50));
  ExitButton.setTitle_forState(NSString.classStringWithUTF8String('Exit'), UIControlStateNormal);
  ExitButton.addTarget_action_forControlevents(self, objcselector('ExitButtonClick:'), UIControlEventTouchUpInside);
  _window.addSubview(ExitButton);

  _window.makeKeyAndVisible;
  Result := True;
end;

procedure TMainForm.applicationWillResignActive(application: UIApplication);
begin
  { send when the application nis about to move from active to inactive state }
end;

procedure TMainForm.applicationDidEnterBackground(application: UIApplication);
begin
  { use this method to release shared resources }
end;

procedure TMainForm.applicationWillEnterForeground(application: UIApplication);
begin
  { called as part of the transition from the background to the inactive state }
end;

procedure TMainForm.applicationDidBecomeActive(application: UIApplication);
begin
  { restarts any tasks that were paused (or not yet started) while the application was inactive }
end;

procedure TMainForm.applicationWillTerminate(application: UIApplication);
begin
  { called when the application is about to terminate.
    Save data if appropriate.
    See also applicationDidEnterBackground:. }
end;

procedure TMainForm.Button1Click(Sender: id);
var r,g,b,a: TCGFloat;
begin
  r := random;
  g := random;
  b := random;
  a := random;
  Label1.setBackgroundColor(UIColor.classColorWithRed_green_blue_alpha(r,g,b,a));
end;

procedure TMainForm.ExitButtonClick(Sender: id);
begin
  Halt(1);
end;

end.

