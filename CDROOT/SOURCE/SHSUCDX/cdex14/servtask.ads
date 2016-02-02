--************************************************************************
--
--  SERVTASK.ADS               Version 3.1
--
--  A copyright-reserved, free use program.
--  (c)John H. McCoy, 1994, 1995, 1996 Sam Houston St. Univ., TX 77341-2206
--************************************************************************

with Types; use Types;
with NetBios; use NetBios;
with Drivers; use Drivers;
with system;
with calendar;

package ServerTasks is

task type Nets is
  entry Start(Name: string16);
  entry Call (Ncb: ncbAccess);
  entry Hold;
  entry ShutDown;
  pragma priority(20);
end Nets;
for Nets'storage_size use 1024;

type NetAccess is access Nets;

task type Schedulers is
  entry Listen(Net : NetAccess;
               Ncb : ncbAccess);
  entry ShutDown;
end Schedulers;
for Schedulers'storage_size use 1024;

type SchedulerAccess is access Schedulers;

task type Sessions is
  entry Start(Net       : NetAccess;
              LocalName : string16;
              SubUnits  : byte;
              Scheduler : SchedulerAccess);
end Sessions;
for Sessions'storage_size use 704;
type SessionsAccess is access Sessions;

task type Consoles is
  entry Init(MaxSessions: integer;
             LastCd     : integer;
             ServerName : string16);
  entry SignIn(SessionNumber: byte;
               Client       : String16);
  entry SignOut(SessionNumber: byte);
  entry CkIn(SessionNumber: byte);
  entry ToggleDriveUpdate;
  entry ShutDown;
end Consoles;
for Consoles'storage_size use 1024;
Console : Consoles;

end ServerTasks;