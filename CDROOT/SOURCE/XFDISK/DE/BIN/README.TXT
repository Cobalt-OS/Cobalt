XFDisk I Version 0.9.1 Beta DE
==============================================================================
XFDisk steht unter der GNU Public Licence (GPL). Die GPL, den Quellcode von
XFDisk, die aktuelle Version sowie weitere Hilfe erhalten Sie auf der
XFDisk Homepage http://home.pages.de/~xfdisk

Windows 2000: XFDisk kann keine Partitionen vor Windows 2000 verstecken.
Der Grund ist, dass Windows 2000 auf alle Partitionen unabhaengig vom
Partitionstyp zugreift. Die einzige Moeglichkeit, Partitionen unter Windows
2000 auszublenden, ist, ihnen keinen Laufwerksbuchstaben zuzuweisen. Dies
kann in den Systemeinstellungen unter Systemsteuerung -> Verwaltung ->
Computerverwaltung -> Datenspeicher -> Datentraegerverwaltung getan werden.


Beschreibung der Kommandozeilenparameter

XFDisk koennen beim Start bestimmte Kommandozeilenparameter uebergeben werden,
die waehrend dem Programmlauf selbst zur Zeit nicht umgestellt werden koennen.
Dieselben Parameter lassen sich in der Datei XFDISK.INI einstellen, damit sie
bei jedem Start benutzt werden. Folgende Parameter werden zur Zeit unter-
stuetzt: 
/AUTOHIDE
  XFDisk erlaubt pro Festplatte maximal eine sichtbare primaere Partition.
  Zusaetzliche primaere Partitionen werden automatisch versteckt. Diese
  Betriebsart entspricht dem Verhalten von XFDisk bis einschliesslich Version
  0.8.5. Ohne Angabe dieses Parameters versteckt XFDisk von sich aus keine
  Partitionen mehr. Damit sollten die meisten Betriebssysteme einschliesslich
  MS-DOS ab Version 5.0 klarkommen. Die zusaetzlichen Partitionen erhalten
  die freien Laufwerksbuchstaben hinter dem letzten logischen Laufwerk der
  letzten Festplatte.
/SORT
  Die Eintraege der Partitionstabelle werden nach der Lage der Partitionen
  sortiert. Das entspricht dem Verhalten von XFDisk bis einschliesslich
  Version 0.8.5. Vorsicht: Linux und Windows NT beziehen sich teilweise auf
  die Partitionsnummern!
/NOWIN9X, /OS2, /WINNT und /LINUX
  Erweiterte Partitionen oberhalb der 8 GB Grenze erhalten normalerweise den
  Partitionstyp 0xF anstatt 0x5. Dadurch greift DOS/Windows nur mit den
  erweiterten BIOS-Funktionen auf diese Partition zu. Da OS/2 diesen
  Partitionstyp offenbar nicht kennt, kann man durch diese Schalter erreichen,
  dass trotzdem der Typ 0x5 benutzt wird. Ausserdem wird beim Anlegen neuer
  Partitionen der Partitionstyp entsprechend dem Wunsch-Betriebssystem gesetz.
/MBR
  schreibt den Master-Boot-Record der ersten Festplatte neu
/NOCOLOR
  schwarz/weiss
/BLIND
  Hardcursor fuer Blinde
/BUILD
  Gibt die Buildnummer von XFDisk aus. Fuer Entwickler und Testkaninchen.
==============================================================================
Ulrich Mueller, 14.09.2000



XFDisk I Version 0.8.5 Beta DE
==============================================================================
User Dokumentation



Inhalt

Vorwort

1. Einleitung
1.1 Was ist XFDisk?
1.2 Eine Warnung vorweg!
1.3 Fuer Eilige
1.4 Installation
1.5 Systemvoraussetzungen

2. Erste Schritte
2.1 XFDisk und seine Bestandteile
2.2 Was sind Partitionen und logische Laufwerke?

3. Bedienung
3.1 Die Oberflaeche
3.2 Arbeiten mit XFDisk
3.2.1 Partitionierung der Festplatten
3.2.2 Installation und Konfiguration des Bootmanagers
3.2.3 Laden und Sichern von Konfigurationen
3.2.4 Weitere Funktionen von XFDisk
3.2.5 Benutzung des Bootmanagers
3.3 Blindenunterstuetzung

4. XFDisk und mehrere Betriebssysteme
4.1 DOS und Windows 95
4.2 Zusaetzliche Informationen zu Windows 95
4.3 Windows NT
4.4 OS/2
4.5 Linux

Anhang
A. Partitionstypen
B. Beispiel fuer eine DOS/Windows 95 Installation
C. Beispiel fuer eine lilo.conf
D. Dateien im Originalarchiv
E. Freewarehinweis
F. Garantiehinweis
G. Eingetragene Warenzeichen
H. Danksagungen


Vorwort
=======

Diese Dokumentation ist so ausgelegt, dass auch Anfaenger mit XFDisk
zurechtkommen sollten. Falls Sie sich schon mit FDISK und Partitionen
auskennen und nur das Notwendigste fuer den Einsatz von XFDisk wissen
moechten, sollten Sie wenigstens Kapitel 1.3 und die darin genannten Kapitel
lesen.

Achtung: Was auf jeden Fall jeder Benutzer, egal ob erfahren oder nicht,
         sorgfaeltig durchlesen sollte, ist Kapitel 1.2, das auf die Gefahren
         bei der Benutzung von XFDisk hinweist. Ebenfalls wird an dieser
         Stelle auf die Anhaenge I und J und die Datei GPL.TXT verwiesen.
         XFDisk darf nur unter Anerkennung der dort aufgefuehrten Bedingungen
         verwendet und weitergegeben werden.


1. Einleitung
=============

1.1 Was ist XFDisk?
---------------------------

Bei XFDisk handelt es sich um ein bequemes und leistungsfaehiges DOS FDISK-
Derivat, das dem "Look & Feel" von OS/2 FDISK nachempfunden ist. Mit XFDisk
koennen komfortabel Partitionen erstellt und verwaltet werden. (Falls Sie
nicht wissen was Partitionen sind, brauchen Sie trotzdem das Programm nicht
gleich wieder zu loeschen; Partitionen werden ausfuehrlich im Kapitel 2.2
erklaert). Des weiteren ist mit XFDisk das Verwenden von mehreren
Betriebssystemen auf einem Rechner moeglich. Dies wird durch den Bootmanager,
einem Bestandteil von XFDisk, ermoeglicht. Eine vollstaendigere Beschreibung
von XFDisk, die detaillierter auf die Bestandteile von XFDisk eingeht, finden
Sie im Kapitel 2.1.


1.2 Eine Warnung vorweg!
------------------------

XFDisk befindet sich noch im Beta-Stadium und ist keinesfalls frei von
Fehlern. Da sowohl durch XFDisk als auch durch den Bootmanager direkt auf die
Festplatte zugegriffen wird, kann es bei Auftreten von Fehlern oder durch
unsachgemaesse Bedienung zu unwiederbringlichen Datenverlusten kommen!
Der Bootmanager belegt die ersten 17 Sektoren Ihrer Festplatte, ein Bereich,
der normalerweise unbenutzt ist. Wenn allerdings ein anderes Programm diesen
Bereich bereits benutzt, wie es z.B. einige Programme zur Verwaltung von
grossen Festplatten tun, kommt es bei der Installation des Bootmanagers
unweigerlich zu Konflikten oder zu totalem Datenverlust.
Der Programmierer wird weder fuer unmittelbar durch das Programm entstandene
noch fuer Folgeschaeden irgendwelcher Art aufkommen. Lesen Sie hierzu auch
Anhang J sowie die Datei GPL.TXT.

!!! SICHERN SIE UNBEDINGT IHRE DATEN, BEVOR SIE DIESES PROGRAMM VERWENDEN !!!

Falls Ihnen Fehler oder Ungereimtheiten unterkommen, waere ich sehr dankbar,
wenn Sie mich ueber die Art der Fehler benachrichtigen wuerden. Eine genauere
Beschreibung, wie Sie einen Bugreport erstellen finden Sie im Anhang D.


1.3 Fuer Eilige
---------------

Die Dokumentation von XFDisk enthaelt einige Kapitel, die ein paar, fuer einen
erfahrenen Anwender auf den ersten Blick banale Techniken erklaeren. Falls Sie
wissen was logische Laufwerke, primaere und erweiterte Partitionen sind und
schon mit OS/2 FDISK gearbeitet haben, brauchen Sie nur die Kapitel 1.2, 3.1,
3.2.1 und 3.2.2 durchzulesen, um XFDisk einsetzen zu koennen.
Diese sind alle gleichermassen wichtig, da XFDisk nicht nur Gemeinsamkeiten
mit dem OS/2 FDISK aufweist, sondern zum Teil auch recht deutliche
Unterschiede dazu.

Fuer den Fall, dass Sie wissen moechten, was bei der Installation bestimmter
Betriebssysteme zu beachten ist, koennen Sie auch im 4. Abschnitt die fuer Sie
in Betracht kommenden Unterkapitel lesen.


1.4 Installation
----------------

Entpacken Sie zunaechst das Archiv XFDnnn.ZIP (nnn ist die Versionsnummer des
Programms), das alle fuer XFDisk notwendigen Dateien enthaelt. Falls Sie
XFDISK.EXE in ein separates Verzeichnis kopieren wollen, sollten sich alle im
Originalarchiv enthaltenen Datei im selben Verzeichnis befinden, da sonst
eventuell einzelne Funktionen nicht zur Verfuegung stehen.
(Informationen zu den im Originalarchiv enthaltenen Dateien finden Sie im
Anhang E. Das Versionsschema wird im Anhang F erklaert).


1.5 Systemvoraussetzungen
-------------------------

XFDisk stellt folgende Anforderungen an die Hardware und das Betriebssystem:

   IBM AT Kompatibler Computer (mindestens 80286 Prozessor)
   Festplatte (Steuerung ueber INT13, also BIOS oder entsprechende Treiber)
   HGC, MDA, EGA oder VGA Karte (80x25 Display)
   DOS 3.3 oder hoeher oder Windows 95

XFDisk funktioniert nicht oder nur eingeschraenkt unter Windows NT, OS/2,
Linux, oder anderen Systemen, die nur eine DOS-Emulation zur Verfuegung
stellen, da hier der direkte Zugriff auf die Hardware moeglicherweise nur
eingeschraenkt oder gar nicht moeglich ist. Wenn zumindest lesender Zugriff
auf die Hardware gestattet ist, kann XFDisk zum Anzeigen der Partitionsdaten
verwendet werden. Versuchen Sie in keinem Fall, Partitionen zu erstellen, zu
loeschen oder zu aendern, den Bootmanager zu installieren oder zu
konfigurieren, da es durch unzulaengliche Kontrollmechanismen innerhalb der
Betriebssysteme zu unwiederbringlichen Datenverlusten kommen kann.

Achtung: Verwenden Sie XFDisk deshalb nur in einem reinen DOS System, wenn
         Sie Aenderungen vornehmen wollen.

Darueber hinaus kann XFDisk auch in einer DOS-Box unter Windows 95 ausgefuehrt
werden, es werden Betriebssystemeigene Mittel verwendet, um den vollen
Zugriff auf die Hardware zu ermoeglichen.


2. Erste Schritte  
=================

2.1 XFDisk und seine Bestandteile
---------------------------------

Da XFDisk mehrere Programme unter einer Oberflaeche zusammenfasst, besteht es
natuerlich auch aus mehreren Komponenten, die unabhaengig von einander
arbeiten. Hier ist die genaue Beschreibung dieser Bestandteile:

1. Zuallererst besteht XFDisk aus einem komfortablen FDISK-Derivat fuer DOS.
   Um die Bedienung von XFDisk zu erleichtern wurde sie dem OS/2 FDISK
   nachempfunden. Mit Hilfe von XFDisk ist es moeglich, Festplatten in
   einzelne Bereiche, sogenannte Partitionen und logische Laufwerke zu
   unterteilen (Partitionen und logische Laufwerke werden im Kapitel 2.2
   genauer erklaert). Durch diese Einteilung ist es moeglich, mehrere
   Betriebsysteme auf einer Festplatte unterzubringen. Auch Dateibereiche
   koennen dadurch getrennt werden.
2. Der zweite und nicht zu unterschaetzende Programmteil von XFDisk ist der
   Bootmanager. Erst durch den Bootmanager ist es moeglich mehrere
   Betriebssysteme unabhaengig voneinander verwenden zu koennen. Bei jedem
   Hochfahren des Rechners ist es moeglich ein Betriebssystem (momentan sind
   maximal 8 Eintraege erlaubt) auszuwaehlen und zu booten. Mit der F1-Taste
   ist auch das Booten von Diskette moeglich. Der Bootmanager wurde ebenfalls
   dem OS/2 Boot manager aehnlich gestaltet.
3. Der dritte und letzte Bestandteil von XFDisk ist der MBR (Master Boot
   Record). Dieser sorgt dafuer, dass beim Booten der Bootmanager geladen
   wird. Sollte der Bootmanager nicht gefunden werden, sorgt der MBR dafuer,
   dass das Betriebssystem auf der aktiven Partition der ersten Platte
   gestartet wird.


2.2 Was sind Partitionen und logische Laufwerke?
------------------------------------------------

Achtung: Anfaenger sollten sich zunaechst einmal mit einen Freund oder
         Bekannten, der mit der Materie vertraut ist, beraten, ob der Einsatz
         des Programmes XFDisk und des Bootmanagers ueberhaupt fuer den
         eigenen Rechner sinnvoll sind.

FDISK, ein Wort, ein Fremdwort, ein Programm von dem schon jeder, egal ob
erfahrener Anwender oder Neuling, gehoert hat. Aber nur die wenigsten Leute
koennen sich unter FDISK etwas konkretes Vorstellen. FDISK wird bei der
Erstinstallation von DOS oder Windows 95/NT gestartet um die Festplatte auf
die neuen Betriebsysteme vorzubereiten, FDISK gibt es fuer jedes Betriebsystem
auf dem PC, FDISK ist zwar sehr alt, wird aber immer noch ueberall gebraucht.
Nicht einmal neuere Betriebsystem wie Windows 95/NT, Linux, OS/2 kommen ohne
dieses Werkzeug aus.
Also was verdammt noch mal ist dieses FDISK ueberhaupt? Die Frage kann einfach
und mit wenigen Worten, und gleichzeitig kompliziert mit langen Erklaerungen
beantwortet werden. Hier ist zuerst die einfache Antwort: FDISK bereitet, wie
schon vorher gesagt, die Festplatte auf neue Betriebssysteme vor, es teilt
sie in mehrere Bereiche auf. Leider hilft diese Antwort kaum einem weiter,
wenn man FDISK bzw. XFDisk einsetzten will. Denn um diese Programme sinnvoll
einsetzten zu koennen, muss man leider erst die langen Erklaerungen verstehen.
Dies ist auch das Ziel dieses Kapitels: die Technik, die hinter der
sogenannten Partitionierung, dem Einteilen einer Festplatte in mehrere
Bereiche, und dem Programm FDISK bzw. XFDisk steht, zu erklaeren.

Wer denkt, dass eine neue Festplatte direkt benutzbar ist, der taeuscht sich.
Ausser dem obligatorischem Formatieren, dass jedes Betriebssystem erledigt,
muss die Festplatte zuerst partitioniert werden, damit sie benutzt werden
kann. Auch wenn Sie den ganzen Platz auf dieser Festplatte einem einzigen
Laufwerksbuchstaben zuteilen und sie nicht in mehrere Bereich aufteilen, muss
die Festplatte trotzdem partitioniert werden. "Was sind eigentlich
Partitionen?", werden sich die meisten Leser jetzt vermutlich fragen.
Die folgenden Erlaeuterungen sollen die Antwort auf diese Frage liefern.

Durch Partitionen ist es moeglich, eine Festplatte in mehrere Bereiche
aufzuteilen, die von einander unabhaengig sind. Fuer das Einrichten von
mehreren Partitionen auf einer Festplatte sprechen mehrere gewichtige Gruende:

1. Zu einem dient die Aufteilung der Festplatte in mehrere Bereiche der
   Datensicherheit. Falls ein Bereich der Festplatte kaputt geht, koennen die
   anderen immer noch benutzt werden da die Partitionen voneinander
   unabhaengig sind.
2. Der zweite Grund ist die Ordnung. Es ist sinnvoll, eine Festplatte zu
   partitionieren um die einzelnen Bereiche jeweils einem Thema zu widmen. Es
   doch offensichtlich, dass man sich D: als den Bereich fuer Arbeitsdokumente
   besser merken kann als C:\WINWORD\TEXT\DOKUMENT\ARBEIT o.ae. Dieser Grund
   wird immer wichtiger, weil das "Nichtuntergehen in der Datenflut" immer
   mehr an Bedeutung zunimmt, da die neuen Festplatten immer
   unueberschaubarere Groessen annehmen.
3. Erst durch die Unterteilung einer Festplatte in mehrere Partitionen koennen
   mehrere Betriebssysteme auf einer Festplatte friedlich koexistieren. Dies
   hat nichts mehr mit den Exoten vergangener Tage wie MINIX o.ae. zu tun.
   Sogar um DOS und Windows 95 vernuenftig nebeneinander einsetzen zu koennen,
   braucht man zwei Partitionen.
4. Bei der Benutzung von DOS und dem FAT-Dateisystem sollte man immer drauf
   achten, dass man die Partitionsgroesse unter 512MB haelt, sonst wird bei
   kleineren Dateien sehr viel Platz verschwendet.

Bisher wurden alle Unterteilungen der Festplatte als Partitionen bezeichnet,
dies wurde nur dazu benutzt um diese Bereiche unter einem einheitlichen
Begriff zusammenzufassen und Ihnen zunaechst klar zu machen wie man diese
Unterteilungen sinnvoll einsetzt.

Nachdem Sie die Grundlagen (hoffentlich) verstanden haben, sollten wir diesen
Begriff etwas genauerer ins Augenschein nehmen; Unterteilung ist naemlich
nicht gleich Unterteilung.

Zunaechst zu den "echten" Partitionen. Nur 4 Partitionen koennen auf einer
Festplatte eingerichtet werden. Diese Einschraenkung kommt daher, dass alle
Informationen ueber die Partitionen zusammen mit dem MBR-Code in einem Sektor
der Festplatte (also auf exakt 512 Byte) untergebracht werden muessen. Der MBR
sorgt bei jedem Start dafuer, dass das Betriebssystem auf der aktiven
Partition der ersten Platte geladen wird oder - im Falle von XFDisk - der
Bootmanager gestartet wird. Falls Sie mehr als vier Laufwerke einrichten
moechten, nicht verzagen und einfach den Text weiterlesen.

Man unterscheidet bei den Partitionen zwischen zwei Typen:

- primaere Partitionen
- erweiterte Partitionen

Die primaeren Partitionen sind fuer das Verwenden von mehreren
Betriebssystemen auf einer Festplatte gedacht. Alle Betriebssysteme, egal ob
DOS, Windows 95, Windows NT, OS/2 oder Linux, lassen sich in jedem Fall auf
einer primaeren Partitionen installieren und von dieser booten, wenn diese
"sichtbar" und "aktiv" ist und sich auf der ersten Festplatte befindet. Leider
gibt es einige Einschraenkungen bei primaeren Partitionen. Fuer viele
Betriebssysteme gilt naemlich, dass nur eine primaere Partition pro Festplatte
"sichtbar" sein darf und benutzt werden kann. Das "Sichtbar machen" und
"Aktivieren" der ausgewaehlten Partition (und damit auch das "Verstecken" und
"Deaktivieren" aller anderen Partitionen), sowie das Booten des gewuenschten
Systems wird vom Bootmanager erledigt und ist fuer Sie transparent.
Leider sind die Schwierigkeiten beim Benutzen damit nicht aus dem Weg
geschafft, denn Sie koennen beim Arbeiten mit dem gebootetem System nur auf
die jeweils "sichtbare" primaere Partition zugreifen, alle anderen sind fuer
Sie in diesem Moment "unsichtbar" - oder "versteckt", wie es in der
Terminologie von XFDisk heisst. Das bedeutet, dass Sie auf den anderen,
"versteckten" primaeren Partitionen keine Dateien lesen, schreiben, kopieren
oder erstellen koennen, da sie nicht ueber einen Laufwerksbuchstaben
angesprochen werden koennen. Die primaeren Partitionen anderer Festplatten
koennen jedoch weiterhin benutzt werden, allerdings auch wieder nur eine pro
Festplatte. In den Kapiteln 3.1 und 3.2.4 erfahren Sie mehr ueber den Status
von Partitionen.

Jetzt kommen die erweiterten Partitionen ins Spiel. Pro Festplatte kann genau
eine erweiterte Partition angelegt werden. Diese erweiterte Partition ist
keine Partition im herkoemmlichen Sinne. Durch die erweiterte Partition ist es
moeglich, theoretisch beliebig viele Bereiche, sogenannte logische Laufwerke,
zu erstellen. Dies ist nur durch das Alphabet beschraenkt, da unter DOS
Laufwerke ueber Buchstaben (A-Z) angesprochen werden. Diese Bereiche sind in
allen Betriebssystemen sichtbar und es kann immer auf sie zugegriffen werden
(sofern das jeweilige Betriebssystem ueberhaupt mit der vorhandenen
Formatierung etwas anzufangen weiss).
Eine erweiterte Partition ist also eine Art Mantel, der einen
zusammenhaengenden Bereich isoliert und ihn dadurch von anderen unabhaengig
macht. In diesem abgegrenzten Bereich koennen logischen Laufwerke als
Unterbereiche erstellt werden.  Diese "Laufwerke" unterscheiden sich
aeusserlich nicht von den "primaeren Partitionen" und werden wie normale
physische Laufwerke behandelt, jedes erhaelt einen Laufwerksbuchstaben. Fuer
XFDisk-Benutzer sind erweiterte Partitionen transparent, denn es koennen
sofort logische Laufwerke eingerichtet werden. Eine separate Erstellung der
erweiterten Partitionen ist nicht notwendig und sogar nicht moeglich, weil
ueberfluessig. Sie muessen bloss drauf achten, dass alle logischen Laufwerke
auf einer Festplatte gruppiert sind. Das heisst, dass die logischen Laufwerke
nicht durch eine primaere Partition unterbrochen werden koennen. Falls dies
passiert, koennen Sie keine logischen Laufwerke mehr erstellen, da nur eine
erweiterte Partition pro Festplatte erlaubt ist.

Als letzte Anmerkung sollte noch gesagt werden, dass primaere Partitionen
immer die groessere Prioritaet bei der Verteilung der Laufwerksbuchstaben
erhalten. Eine primaere Partition der zweiten Festplatte wird nach der
primaeren Partition der ersten Festplatte, aber vor den logischen Laufwerken
eingegliedert.

Beispiele:

Nehmen wir mal an, dass Sie die Festplatte so partitioniert haben:
	
   Nummer   Bezeichnung   Art
   1        MS Windows    primaere Partition
   2        MS DOS        primaere Partition
   3        Programme     logisches Laufwerk 
   4        Spiele        logisches Laufwerk
   5        Dokumente     logisches Laufwerk

Beim booten von Windows 95 wuerde die Verteilung der Laufwerksbuchstaben wie
folgt aussehen:

   Laufwerk   Bezeichnung
   C:         MS Windows
   D:         Programme
   E:         Spiele
   F:         Dokumente  

Beim booten von DOS wuerde das Ganze allerdings so aussehen:

   Laufwerk   Bezeichnung
   C:         MS DOS
   D:         Programme
   E:         Spiele
   F:         Dokumente  

Beispiele mit mehreren Festplatten:

   Festpl.   Nummer   Bezeichnung   Art
   1         1        MS Windows    primaere Partition
             2        MS DOS        primaere Partition
             3        Dokumente     logisches Laufwerk
             4        Programme     logisches Laufwerk

   2         1        Daten         primaere Partition
             2        Spiele        logisches Laufwerk
      
Beim Booten von Win95/DOS wuerde die Verteilung der Laufwerksbuchstaben wie
folgt aussehen:

   Laufwerk   Bezeichnung
   C:         MS DOS/MS Windows (siehe erstes Beispiel)
   D:         Daten
   E:         Dokumente
   F:         Programme
   G:         Spiele  

Durch die Tatsache, dass nur eine primaere Partition pro Festplatte sichtbar
ist, eigenen sich primaere Partition eher fuer  Daten, auf die nur jeweils
exklusiv zugegriffen werden sollte, wie beispielsweise Betriebssysteme.
Logische Laufwerke eignen sich dagegen fuer die Speicherung von Daten, die
gemeinsam (z.B. von mehreren Betriebssystemen) benutzt werden. Neuere
Betriebssystemen wie Linux, FreeBSD und OS/2 lassen sich auch auf ein
logisches Laufwerk oder eine zweite Festplatte installieren, so dass die
Beschraenkung von vier Partitionen auf der ersten Festplatte und damit maximal
vier Betriebssystemen umgegangen werden kann.

Achtung: Hier sei noch einmal explizit darauf hingewiesen, dass DOS und damit
         auch Windows 95 eine primaere Partition auf der ersten Festplatte
         benoetigen, um dort ihre Systemdateien unterzubringen. Obwohl DOS bei
         einem "SYS D:" oder "FORMAT D: /S" keine Fehlermeldung ausgibt, kann
         von Laufwerk D: niemals gebootet werden. Das selbe gilt, wenn auch
         aus anderen Gruenden, fuer Windows NT.

Anschliessend ist noch zu sagen, dass eine erweiterte Partition als eine
"normale" Partition zaehlt. Wenn Sie eine erweiterte Partition einrichten,
also ein oder mehrere logische Laufwerke erstellen, koennen sie nur noch drei
primaere Partitionen auf einer Festplatte erstellen. Wenn die Anzahl der
Partitionen gleich vier ist und immer noch unzugeordneter Speicherplatz auf
der Festplatte verbleibt, kann dieser normalerweise nicht verwendet werden.

Achtung: Falls Sie ihre Festplatte umpartitionieren moechten, sollten Sie
         bedenken, dass alle sich in den geloeschten Partitionen befindenden
         Dateien verloren gehen.  Wenn Sie nur eine Partition auf ihrer
         Festplatte haben und diese umpartitionieren, gehen alle Dateien auf
         der Festplatte verloren.

Bei der Installation des Bootmanagers sollte es normalerweise nicht zu
Datenverlust kommen (siehe  auch Anhang J).


3. Bedienung
============

3.1 Die Oberflaeche
-------------------

Als erstes begruesst Sie XFDisk mit einer Liste der vorhandenen Partitionen
und logischen Laufwerken der ersten Festplatte. Falls Sie mehrere Festplatten
eingebaut haben, koennen Sie mit der Tabulatortaste zwischen den einzelnen
Festplatten umschalten. Die Nummer der aktuellen Festplatte wird oben links
im Fenstertitel gezeigt.
Unter dem Titel sehen Sie eine Auflistung aller Partitionen und logischer
Laufwerke auf der aktuellen Festplatte. Die Auflistung ist in mehrere
Abschnitte geteilt.

Der Abschnitt "Name" steht im Zusammenhang mit dem Bootmanager. Hier wird der
von Ihnen gewaehlte Name der Partition oder des logischen Laufwerks angezeigt,
wie er spaeter auch im Menue des Bootmanagers erscheint.

"Lw" steht fuer Laufwerk und zeigt Ihnen die aktuelle Zuordnung der
Partitionen und logischen Laufwerke zu Laufwerksbuchstaben unter DOS an
(siehe hierzu auch Kapitel 2.2).

Im dritten Abschnitt wird der Status der zugehoerigen Partition angezeigt.
Der Status kann "aktiv", "inaktiv" oder "versteckt" sein, die jeweils
"aktive" Partition der ersten Festplatte wird beim Einschalten des Computers
normalerweise automatisch gebootet, wenn sie ein gueltiges Betriebssystem
enthaelt. Wird hier "N/A" angezeigt, was fuer "not available" steht, so ist
der betreffende Eintrag ein logisches Laufwerk oder ein freier Bereich auf der
Festplatte.
Betrachten Sie bitte den Status "versteckt" immer getrennt von den uebrigen.
Eine "versteckte" Partition kann sowohl "aktiv" als auch "inaktiv" sein, im
Normalfall macht jedoch eine "aktive", "versteckte" Partition keinen Sinn.
Achten Sie also darauf, dass keine "versteckte" Partition "aktiv" ist, da
ansonsten eventuell der Computer nicht richtig bootet. Dies ist leicht zu
ueberpruefen, da nur eine Partition pro Festplatte "aktiv" sein kann und der
Status "aktiv" oder "inaktiv" nur angezeigt wird, wenn die Partition auch
"sichtbar" ist. Ist die Partition "versteckt", wird immer der Status
"versteckt" angezeigt, egal ob die betreffende Partition "aktiv" oder
"inaktiv" ist und umgelehrt ist eine Partition immer sichtbar, wenn der
Status "aktiv" oder "inaktiv" angezeigt wird. Es muss also in der Liste der
Partitionen genau einmal der Status "aktiv" auftauchen (im Normalfall nimmt
Ihnen XFDisk diese Arbeit ab).

Der vierte Abschnitt zeigt die Art des jeweiligen Eintrages an. Dabei steht
"Primaer" fuer eine Partition (zur Unterscheidung von der erweiterten
Partition, die zur Verwaltung der logischen Laufwerke noetig ist) und
"Logisch" fuer ein logisches Laufwerk. Ist der betreffende Eintrag ein freier
Bereich, so gibt dieser Abschnitt an, welche Art der Bereich zur Verwendung
annehmen kann. Hier kommen noch zwei weitere Bezeichnungen zum Einsatz.
"Pri/Log" gibt an, dass der Bereich sowohl fuer logische Laufwerke als auch
fuer Partitionen verwendet werden kann. Ein Bereich, der mit "N/A"
gekennzeichnet ist, kann nicht weiter verwendet werden (fuer Gruende, warum
dies der Fall sein kann, lesen Sie bitte auch Kapitel 2.2).

"Groesse" gibt logischerweise die Groesse des Bereiches an.

Unter "System" wir die Formatierung des jeweiligen Bereiches eingetragen.
Steht hier "N/A" zusammen mit einer Typangabe, so ist das jeweilige
Dateisystem XFDisk unbekannt (was aber nur ein kosmetisches Problem darstellt
und die Verwendung nicht weiter einschraenkt). Sollte hier "ungueltig"
angezeigt werden, dann ist irgend etwas faul und die betreffende Partition
sollte besser geloescht und neu angelegt werden, nachdem Sie die darauf
befindlichen Daten gesichert haben.

Der letzte Abschnitt schliesslich bezeichnet das Label, dass Sie dem Laufwerk
bzw. der Partition bei der Formatierung oder durch den DOS-Befehl "LABEL"
zugewiesen haben.

Ist eine Partition neu, das heisst, haben Sie sie mit XFDisk angelegt oder
ihren Typ geaendert, dann wird sie hervorgehoben angezeigt. (Lesen Sie hierzu
auch Kapitel 3.2.3 und 3.2.4.)

Mit den Hoch- und Runter-Tasten (Pfeiltasten) koennen sie eine Partition, ein
logisches Laufwerk oder einen freien Bereich auf der Festplatte markieren und
Aenderungen an dem markierten Bereich vornehmen und Partitionen oder logische
Laufwerke erstellen oder loeschen. Diese Vorgaenge werden in den naechsten
Kapiteln erklaert.


3.2 Arbeiten mit XFDisk
-----------------------

Im folgenden soll der Umgang mit XFDisk und dem Bootmanager naeher erlaeutert
werden.


3.2.1 Partitionierung der Festplatten
-------------------------------------

Markieren Sie mit den Pfeiltasten den Bereich den Sie veraendern wollen, dies
kann eine primaere Partition, ein logisches Laufwerk oder unbenutzter
Speicherplatz sein. Durch das Druecken der Return-Taste erscheint ein Menue.
Das obere Drittel des Menues beinhaltet Optionen zur Partitionierung und
Verwaltung der Partitionen.
Wenn Sie vor dem Druecken der Return-Taste eine vorhandene Partition oder ein
logisches Laufwerk markiert haben, koennen Sie jetzt z.B. die ausgewaehlte
Partition loeschen.
Ausserdem ist es moeglich ueber die Funktion "Partitionstyp aendern" den
Partitionstyp einzustellen. Diese Funktion dient dem Vorbereiten einer neu
angelegten Partition fuer ein bestimmtes Dateisystem (normalerweise ist FAT
voreingestellt). Mit einem Partitionstyp wird die Kennzahl des Dateisystems
bezeichnet.
Die Kennzahlen der haeufigsten Dateisysteme finden Sie im Anhang A.

Achtung: Alle Dateien, die auf der Partition gespeichert waren, gehen beim
         Loeschen derselben oder beim Aendern des Partitionstyps verloren.

Haben Sie jedoch einen freien Bereich markiert, so ist es moeglich, eine
Partition zu erstellen. Wenn Sie diese Option ausgewaehlt haben, werden Sie
nach der Groesse, Art (siehe auch Kapitel 2.2) und Plazierung der Partition
gefragt.

Anders, als Sie das vielleicht von DOS FDISK her gewoehnt sind, werden die
Aenderungen, die Sie vornehmen unter XFDisk nicht sofort ausgefuehrt, sondern
finden nur in den Daten im Speicher statt. Wenn Sie XFDisk mit der F3-Taste
beenden, werden Sie gefragt, ob die Aenderungen durchgefuehrt werden sollen.
Sie haben hier zusaetzlich die Moeglichkeit, die Aenderungen zu verwerfen,
oder aber in das Programm zurueckzukehren, um die Aenderungen nochmals zu
pruefen.
Wenn Sie die Aenderungen beim Beenden sichern, fragt XFDisk evtl. nach, ob der
Computer neu gestartet werden soll. Im Allgemeinen kann man sagen, wenn diese
Abfrage auftaucht, dass es grundsaetzlich sinnvoll ist, den Rechner neu zu
starten. In einigen Faellen kann dies jedoch unerwuenscht sein, deshalb laesst
Ihnen XFDisk hier die Wahl.
Wenn Sie sicher sind, dass Sie die Aenderungen verwerfen wollen, koennen Sie
XFDisk auch mit der Tastenkombination Strg-C abbrechen. Normalerweise ist
dies jedoch nicht notwendig.


3.2.2 Installation und Konfiguration des Bootmanagers
-----------------------------------------------------

Wenn Sie auf einem beliebigen markierten Bereich in XFDisk durch druecken der
Return-Taste  das Menue geoeffnet haben, wird Ihnen sicherlich der Menuepunkt
"Bootmanager installieren" aufgefallen sein. Mit Hilfe dieses Menuepunktes
wird der Bootmanager auf den ersten 17 Sektoren Ihrer Festplatte installiert.
Der alte MBR wird dabei in Sektor 17 gesichert. Sobald der Bootmanager
installiert ist, wird das Menue um einige Punkte erweitert, die den Umgang mit
dem Bootmanager betreffen.

Mit dem Menuepunkt "Neuer Bootmenue Eintrag" koennen Sie eine Partition oder
ein logisches Laufwerk in das Menue des Bootmanagers aufnehmen und einen Namen
vergeben. Der Name wird im ersten Abschnitt des Bildschirmes, wie oben
bereits beschrieben, angezeigt.
Wenn der Bootmanager installiert ist, muessen Sie mindestens einen Bereich in
das Menue des Bootmanagers aufnehmen. Der erste aufgenommene Eintrag wird als
Standard definiert und mit einer "0" vor dem Namen gekennzeichnet. Alle
weiteren Eintraege in das Bootmanager Menue werden fortlaufend numeriert.
Einen einmal angelegten Eintrag in das Menue des Bootmanagers koennen Sie mit
dem Menuepunkt "Bootmenue Eintrag loeschen" wieder entfernen. Dabei wird nur
der betreffende Eintrag aus dem Menue des Bootmanagers entfernt, nicht jedoch
die Partition bzw. das logische Laufwerk geloescht. Sie koennen auch den
Namen, den Sie dem Bereich gegeben haben, wieder aendern. Dies erreichen Sie
ueber den Menuepunkt "Bootmenue Eintrag aendern".
Es wird jeweils nur der fuer den Bereich passende Menuepunkt angezeigt.

Ueber den Menuepunkt "Startwerte festlegen" erreichen Sie ein Untermenue, mit
dem Sie das Zeitlimit fuer den automatischen Bootvorgang festlegen, die
Standard Partition aendern und das Verhalten des Bootmanagers weitergehend
Ihren Beduerfnissen anpassen koennen. Damit hat es folgendes auf sich:
Der Bootmanager wird beim Einschalten des Computers automatisch gestartet und
wartet auf die Auswahl des Benutzers um ein Betriebssystem zu booten. Damit
der Computer nicht ewig wartet, kann der Benutzer ein Zeitlimit festlegen,
nach dessen Ablauf der Bootmanager einen bestimmten Eintrag des Menues, den
sogenannten Standard-Eintrag, automatisch bootet.
Normalerweise wird der erste in das Menue aufgenommene Eintrag als Standard
gewaehlt, Sie koennen aber jeden beliebigen anderen Eintrag als Standard
festlegen und auch das Zeitlimit bis zu 300 Sekunden einstellen, das der
Bootmanager auf eine Eingabe warten soll. (Eine 0 steht dabei fuer kein
Zeitlimit: Der Computer wartet dann unbeschraenkte Zeit auf Ihre Eingabe.)
Wahlweise merkt sich der Bootmanager auch die beim Booten gewaehlte Partition
und bietet sie beim naechsten Start als Standard an. Die Reihenfolge der
Eintraege im Bootmenue wird dadurch nicht geaendert.
Um den Standard-Eintrag zu aendern, markieren Sie einen Bereich, den Sie
schon in das Menue des Bootmanagers eingetragen haben und den Sie als neuen
Standard-Eintrag haben moechten, und druecken die Return-Taste. Im Menue
waehlen Sie "Startwerte festlegen" und dann in dem aufklappenden Untermenue
den Punkt "Standard Partition". Der neue Standard-Eintrag wird mit einer "0"
vor dem Namen gekennzeichnet. Die restlichen Eintraege im Bootmanager Menue
werden entsprechend umplaziert. Um die jeweils vorherige Partition beim Start
als Standard zu waehlen, aktivieren Sie die Option "Auswahl merken".
Alternativ zur Standardpartition koennen Sie die Partition, die beim naechsten
Start gebootet werden soll, auch ueber die Optionen "Auswahl merken" und
"Diese Partition auswaehlen" festlegen, ohne dass sich die Reihenfolge der im
Bootmenue angezeigten Partitionen aendert.

Weitere Informationen zur Konfiguration des Bootmanagers finden Sie im Kapitel
3.2.4.

Auch die Konfiguration des Bootmanagers findet nur im Speicher statt und kann
beim Beenden des Programmes mit der F3-Taste gesichert oder verworfen werden.
Nur gibt es hier ein paar Ausnahmen: Die Installation, das Entfernen und das
Aktualisieren des Bootmanagers finden grundsaetzlich sofort nach der Wahl des
jeweiligen Menuepunktes statt. Trotzdem koennen Sie auch hier das Programm mit
der Tastenkombination Strg-C abbrechen, ohne dass die vorgenommenen
Aenderungen gesichert werden.


3.2.3 Laden und Sichern von Konfigurationen
-------------------------------------------

Mit den Menuepunkten "Konfiguration laden" und "Konfiguration sichern" koennen
Sie die gesamte mit XFDisk erstellte oder angezeigte Konfiguration aus einer
Datei laden bzw. in eine Datei sichern. Damit koennen mehrere Rechner exakt
gleich Partitioniert und mit Bootmanager versehen werden oder es kann eine
alte Konfiguration wiederhergestellt werden.

Voraussetzung hierfuer ist allerdings, dass die Informationen ueber vorhandene
Festplatten mit denen in der Datei uebereinstimmen. Das heisst konkret, dass
dies nur funktioniert, wenn zwei Rechner exakt die selbe Anzahl und Typen von
Festplatten eingebaut haben, bzw. wenn Sie eine alte Konfiguration
wiederherstellen wollen, dass keine Festplatten nachtraeglich hinzugefuegt
oder entfernt oder durch Festplatten anderen Typs ausgetauscht wurden. Der
Grund fuer diese Einschraenkung ist, dass unterschiedliche Festplatten auch
unterschiedliche Geometrien haben und Partitonsdaten daher nicht einfach
uebertragbar sind.

Wichtig ist in diesem Zusammenhang auch, dass Informationen darueber
gespeichert werden, ob eine Partition neu ist und daher initialisiert werden
soll. Diese Initialisierung ist notwendig, damit die nachfolgende Formatierung
unter DOS korrekt durchgefuerht werden kann. Neue Partitionen werden unter
XFDisk hervorgehoben dargestellt.
Wenn Sie also eine Konfiguration sichern, die neue Partitionen enthaelt, dann
werden diese erneut initialisiert, wenn Sie die Konfiguration erneut laden und
beim Beenden der Programmes auf die Festplatten schreiben. (Wenn Sie eine
Konfiguration lediglich betrachten ohne sie beim Beenden zu schreiben, dann
wird selbstverstaendlich auch keine Partition initialisiert.)
Dies sollten Sie bedenken, wenn Sie Konfigurationen sichern. Ist die
Konfiguration dafuer gedacht, mehrere Rechner jeweils neu zu partitionieren,
dann sollte sie entsprechende neue Partitionen enthalten. Wollen Sie hingegen
eine Konfiguration zur spaeteren Widerherstellung sichern, sollte sie
zweckmaessigerweise keine neuen Partitionen enthalten.


3.2.4 Weitere Funktionen von XFDisk
-----------------------------------

Auch im Zusammenhang mit dem Bootmanager sollten noch weitere Funktionen
erwaehnt werden, die jedoch nicht unmittelbar der Partitionierung dienen oder
fuer die Grundkonfiguration des Bootmanagers notwendig sind und deshalb nicht
in einem der beiden vorangegenagenen Kapitel aufgetaucht sind.

Wenn Sie den Bootmanager nicht installiert haben, oder manuell eine andere
Partition als die aktuell "sichtbare" verwenden wollen, koennen Sie dies ueber
die Menuepunkte "Partition sichtbar machen" und, falls noetig, "Partition
aktivieren" erreichen. Alle anderen Partitionen der jeweiligen Festplatte
werden automatisch "versteckt" bzw. "deaktiviert". Danach muessen Sie beim
Beenden von XFDisk die Partitionstabelle schreiben und den Computer neu
starten. Diese Funktion ist zum Beispiel beim Einrichten mehrerer
Betriebssysteme hilfreich, wenn der Bootmanager noch nicht installiert oder
noch nicht vollstaendig konfiguriert ist.
Beachten Sie zu den Begriffen "aktiv", "inaktiv", "sichtbar" und "versteckt"
bitte auch den entsprechenden Absatz ueber den Partitionsstatus in Kapitel
3.1.

Mit der Funktion "Passwort festlegen" haben Sie die Moeglichkeit, ein
Partitionspasswort zu vergeben, das beim Start des auf der betreffenden
Partition installierten Betriebssystems ueber den Bootmanager abgefragt wird.
Unter dem Menue "Startwerte festlegen", haben Sie die Moeglichkeit, den
Zugriff auf den Computer noch weiter einzuschraenken. Hier koennen noch das
sogenannte Masterpasswort und das Floppypasswort festgelegt werden. Das
Masterpasswort wird automatisch beim jedem Start des Computers noch vor dem
erscheinen des Bootmanager Menues abgefragt (wenn der Bootmanager installiert
ist). Mit dem Floppypasswort koennen Sie die Floppy Boot Funktion (F1-Taste)
des Bootmanagers mit einem Passwort versehen.
Bitte beachten Sie, dass diese Funktionen nicht dazu gedacht sind, alleine
eine Absicherung Ihres Computers zu bieten, sondern nur in Verbindung mit
anderen Massnahmen sinnvoll eingesetzt werden koennen. Hierzu zaehlen
zumindest die Aenderung der Bootreihenfolge im BIOS Setup und ein BIOS
Passwort.
Gehen Sie jedoch mit diesen Massnahmen aeusserst vorsichtig um und verwenden
Sie sie nur, wenn unbedingt notwendig. Wenn Sie ein Passwort vergessen, ist es
unter Umstaenden nicht mehr moeglich, Ihren Computer zu starten, dies gilt
insbesondere fuer Passwoerter, die das BIOS absichern. Notieren Sie sich die
Passwoerter sehr sorgfaeltig und bewahren Sie sie an einem sicheren, fuer
dritte unzugaenglichen Ort auf und Informieren Sie sich vorher ueber die
Moeglichkeiten, die BIOS Passwoerter zu loeschen, falls Sie sie dennoch
vergessen sollten!

Beim Vergeben von Passwoertern sollten Sie auch bedenken, dass normalerweise
beim Start des Computers kein deutscher Tastaturtreiber geladen ist. Verwenden
Sie daher keine Sonderzeichen, die nicht oder nicht einfach ueber die Tastatur
beim Systemstart erreichbar sind oder laden Sie einen amerikanischen
Tastaturtreiber, bevor Sie in XFDisk Passwoerter vergeben.
Die Option, XFDisk die Umsetzung von deutschen Tasten vorzunehmen, wurde
entfernt, da sie zum einen nur eingeschraenkt Fuktionsfaehig war und zum
anderen, weil XFDisk mit zunehmender Popularitaet auch im Ausland fuer jede
lokalisierte Tastur eine Umsetzungstabelle integrieren muesste, was das
Programm nur unnoetig aufblaehen wuerde. Die Funktion kann ueber ein einfaches
KEYB US vor dem Aufruf von XFDisk erreicht werden.

Wenn der Bootmanager versehentlich per FDISK /MBR deaktiviert wurde oder Sie
von einer aelteren Version von XFDisk updaten, koennen sie den Bootmanager
ueber den Menuepunkt "Bootmanager aktualisieren" wieder aktivieren bzw. auf
den neuesten Stand bringen. Die Einstellungen gehen hierbei nicht verloren.
Diese Funktion ermoeglicht auch ein aktualisieren der Informationen des
Bootmanagers falls sich Partitionstypen geaendert haben sollten (wenn die
Aenderungen ueber XFDisk und die Funktion "Partitionstyp aendern" erfolgt ist,
ist dies allerdings nicht notwendig).

Mit Hilfe des Menues "Zeitlimit Verhalten" unter "Startwerte festlegen"
koennen Sie bestimmen, wie der Bootmanager bei Druck auf eine der drei Tasten
Hoch, Runter und F5 das Zeitlimit behandeln soll. Moegliche Einstellungen sind
"Bei Tastendruck deaktivieren", "Bei Tastendruck zuruecksetzen" und
"Tastendruck ignorieren". Deaktivieren bedeutet, bei Druck auf eine der drei
Tasten wird das Zeitlimit deaktiviert, so, als haetten Sie die Escape-Taste
gedrueckt. Wahlweise kann das Zeitlimit auch wieder zurueckgesetzt werden und
von vorne beginnen oder der Tastendruck wird komplett ignoriert. Waehrend der
BIOS Bildschirm mit der F5-Taste eingeblendet ist, wird das Zeitlimit in jedem
Fall voruebergehend ausgesetzt.

Der Bootmanager loescht normalerweise den Hintergrund (BIOS  Bildschirm)
nicht, bevor das BootMenue angezeigt wird. Dies laesst sich jedoch ueber die
Funktion "Bildschirm loeschen" erreichen. Damit wird der Bootmanager vor einem
schwarzen Hintergrund angezeigt, der BIOS Bildschirm bleibt aber weiterhin
ueber die F5-Taste erreichbar und wird vor dem Bootvorgang wieder
eingeblendet.

Sollten Sie die Informationen, die der Bootmanager normalerweise anzeigt,
stoeren oder verwirren, dann koennen Sie ueber die Option "Einfache Menues"
nur die Namen der Eintraege im Bootmenue anzeigen lassen.

Beim Anlegen einer neuen Partition oder Aendern des Partitionstyps haben
Sie die Wahl, ob die Partition initialisiert werden soll. Dies ist notwendig,
falls die Partition anschliessend mit Programmen eines bestimmten Software-
herstellers formatiert werden soll. Eine vollstaendige Initialisierung
bietet ausserdem die Moeglichkeit, dass saemtliche Sektoren der Partition
ueberschrieben werden.
Sowohl bei der teilweisen als auch bei der vollstaendigen Initialisierung
werden bestehende Daten auf der betreffenden Partition unbrauchbar.
Normalerweise spielt dies keine Rolle, da sich mit dem Partitionstyp zumeist
auch die Formatierung aendert, und damit die Daten auf der Partition in jedem
Fall unbrauchbar werden. In einigen Faellen kann es jedoch notwendig
sein, den Partitionstyp zu aendern, ohne dabei die Daten zu zerstoeren.

Mit den Parametern /NOWIN9X, /OS2, /WINNT und /LINUX kann Einfluss auf die
Wahl der standardmaessig verwendeten Partitionstypen genommen werden. Der
Parameter /NOWIN9X unterdrueckt die neuen Partitionstypen, die mit Windows 95
eingefuehrt wurden. Die Parameter /OS2, /WINNT und /LINUX waehlen den fuer
eine neu angelegte Partition vergebenen Typ. Dabei verwendet XFDisk den
Partitionstyp HPFS/NTFS, wenn einer der Parameter /OS2 oder /WINNT angegeben
wurde und den Partitionstyp Linux native, wenn der Parameter /LINUX angegeben
wurde.
Da OS/2 nicht mit den neuen Partitionstypen von Windows 95 umgehen kann,
waehlt der Parameter /OS2 automatisch den Parameter /NOWIN9X mit aus. 
Windows NT und Linux koennen dagegen mit den neuen Partitionstypen umgehen
und der Parameter /NOWIN9X muss, falls gewuenscht, extra angegeben werden.


3.2.5 Benutzung des Bootmanagers
--------------------------------

Wenn Sie den Bootmanager wie in den vorangegangenen Kapiteln installiert
haben, erscheint dieser wie konfiguriert jedesmal beim Einschalten des
Computers oder nachdem der Computer neu gestartet wird.

Mit den Hoch- und Runter-Tasten (Pfeiltasten) koennen Sie die zu bootende
Partition waehlen. Mit der Return-Taste bestaetigen sie Ihre Wahl und der
Bootmanager laed das entsprechende Betriebssystem.

Je nachdem, ob Sie ein Zeitlimit eingestellt haben oder nicht und wie Sie das
Verhalten des Bootmanagers bei einem Tastendruck gewaehlt haben, startet der
Bootmanager nach Ablauf des Zeitlimits das Betriebssystem, das aktuell
ausgewaehlt ist. Normalerweise wird bei Start des Bootmanagers die Partition
vorgewaehlt, die Sie als Standard-Eintrag definiert haben. Wenn Sie das
Zeitlimit deaktivieren wollen, druecken Sie die Escape-Taste.

Mit den Tasten F1 und F2 koennen Sie wahlweise auch nachtraeglich noch von
einer Diskette booten. Der Unterschied dabei ist, dass bei der Wahl von
F1 die Partitionstabelle der Festplatte unveraendert bleibt. Bei F2 wird
die Partitionstabelle genauso angepasst, als wenn die gerade im Menu
gewaehlte Partition gebootet werden soll.

Die F5-Taste bietet Ihnen die Moeglichkeit, den Bootmanager auszublenden und
einen Blick auf die Meldungen des BIOS zu werfen. Dies funktioniert auch
dann, wenn der Bildschirm vor der Anzeige geloescht wurde. Waehrend der BIOS
Bildschirm mit der F5-Taste eingeblendet ist, wird das Zeitlimit
voruebergehend ausgesetzt, damit nicht versehentlich das aktuell gewaehlte
Betriebssystem gestartet wird. Der Bootmanager wird ueber die F5-Taste auch
wieder eingeblendet, alle anderen Tasten werden ignoriert, solange der
Bootmanager ausgeblendet ist.

Gegebenenfalls wird vor der Anzeige des Bootmanagers oder nach der
Bestaetigung zum Booten eines Betriebssystems oder von Diskette das
zugehoerige Passwort abgefragt. Sie haben drei Versuche das Passwort richtig
einzugeben, danach wird der Computer angehalten und muss neu gestartet werden.


3.3 Blindenunterstuetzung
-------------------------

XFDisk wurde fuer blinde Benutzer speziell an die ueblichen
Sprachausgabesysteme angepasst. Benutzen Sie zum Aktivieren der
Blindenunterstuetzung bitte den Parameter /BLIND beim Aufruf von XFDisk von
der Kommandozeile. Die Unterstuetzung ist erst im Aufbau befindlich und wird
in Zukunft noch weiter ausgebaut werden. Besonders der Bootmanager ist derzeit
nur schwer oder gar nicht zu bedienen, hier bietet derzeit vermutlich nur die
Blindenzeile wirkliche Abhilfe.

Die Unterstuetzung beschraenkt sich derzeit im wesentlichen auf spezielle
Tonfolgen, die Hinweise auf den augenblicklichen Zustand des Systems geben.
So bedeutet ein kurzer, hoher Ton, dass XFDisk von Ihnen Eingaben in einer
Dialogbox erwartet. Der Text der Dialogbox kann ueber das Sprachausgabesystem
normalerweise vorgelesen werden. Ein langer, tiefer Ton bedeutet, dass die
Eingabe falsch war (z.B. Passwort falsch, ungueltige Partitionsgroesse) oder
nicht verarbeitet werden konnte.

Im Bootmanager wird der lange, tiefe Ton durch drei aufeianderfolgende Toene
simuliert. Hier gibt es auch noch eine dritte Variante. Zwei Toene
hintereinander bedeuten die Aufforderung zur Eingabe eines Passwortes.
Im allgemeinen wird jedoch von der Benutzung des Bootmanagers abgeraten, weil
z.B. keinerlei Rueckmeldung ueber die aktuelle Auswahl im Menue erfolgt.

Da es speziell fuer Blinde hilfreich sein kann, die beim naechsten Start zu
bootende Partition in XFDisk zu waehlen, ohne dabei die Reihenfolge der im
Bootmenue angezeigten Partitionen zu aendern, sei hier nochmal explizit auf
die Kombination der Optionen "Auswahl merken" und "Diese Partition auswaehlen"
hingewiesen, mit denen genau dies erreicht werden kann, wie in Kapitel 3.2.2
kurz beschireben.


4. XFDisk und mehrere Betriebssysteme
=====================================

4.1 DOS und Windows 95
----------------------

Spaetestens ab Windows 95 ist kein vernuenftiges arbeiten mit einem separatem
DOS moeglich. Wenn man Glueck hat und den Microsoft DOS einsetzt (ist wohl bei
den meisten Anwendern der Fall), so ist ein Booten mit der "vorherigen
DOS-Version" moeglich, obwohl das Ganze sehr unkomfortabel ist. Besitzt man
jedoch z.B. Novell DOS oder den Nachfolger Caldera OpenDOS, so ist das Booten
mit der "vorherigen DOS-Version" ohne zusaetzliche Tools gar nicht moeglich.
Des weiteren kommt hinzu, dass die OSR-2 Version von Windows 95 gar kein
Benutzen des alten DOS erlaubt, weil es einen Bug an dieser Stelle aufweist.

Mit dem Bootmanager ist es moeglich die beiden Betriebssysteme unabhaengig von
einander zu booten. Da jedoch DOS und Windows 95 nur auf einer primaeren
Partition auf der ersten Platte installiert werden koennen, werden die Daten
der DOS Partition in Windows und umgekehrt nicht sichtbar sein. Um diesen
Effekt einzuschraenken lohnt es sich jeweils nur kleine primaere Partitionen
einzurichten und darauf nur zum hochfahren notwendige Dateien zu speichern.
Alle Dokumente und Anwendungsprogramme koennen dann auf logische Laufwerke,
die ja von beiden Systemen aus sichtbar sind, untergebracht werden.
Eine moegliche Vorgehensweise bei der Installation von DOS und Windows 95,
die bei einer Neuinstallation und Neupartitionierung der Festplatte sehr
empfehlenswert ist, finden Sie im Anhang B.


4.2 Zusaetzliche Informationen zu Windows 95
--------------------------------------------

Windows 95 ueberschreibt bei der Installation den MBR, so dass evtl. der
Bootmanager deaktiviert wird. Sie koennen den Bootmanager entweder durch den
Aufruf von XFDisk /MBR oder explizit ueber den Menuepunkt "Bootmanager
aktualisieren" wieder aktivieren, letztere Methode wird empfohlen. Lesen Sie
auch das vorangegangene Kapitel und fuer eine moegliche Vorgehensweise Anhang
B.


4.3 Windows NT
--------------

Windows NT benoetigt in jedem Fall eine primaere Partition auf der ersten
Festplatte, auf der es seine Bootdateien unterbringen kann (NTDETECT.COM und
NTLDR, sowie evtl. BOOT.INI und BOOTSECT.DOS). Windows NT kann daher auch
nicht direkt von einem logischen Laufwerk oder von einer zweiten Festplatte
vom Bootmanager gestartet werden. Der Umweg ueber den Bootmanager von
Windows NT ist hier obligat.

Achtung: Da sich beim Partitionieren mit XFDisk oder beim Installieren des 
         XFDisk Bootmanagers die Reihenfolge der Partitionen aendern kann, 
         muss gegebenenfalls vor diesen Aktionen die Datei BOOT.INI im
         Rootverzeichnis der Bootpartition geaendert werden. XFDisk sortiert
         die Partitionen in ihrer physikalischen Reihenfolge, nicht in der
         Reihenfolge, in der die Partitionen angelegt wurden. Lesen Sie bitte
         hierzu unbedingt die Dokumentation zu Windows NT. Sie koennen das
         Problem vermeiden, wenn Sie zuerst die Festplatte vollstaendig mit
         XFDisk Partitionieren, bzw. die Partitionen in ihrer physikalischen
         Reihenfolge anlegen, bevor Sie Windows NT installieren.

Wenn mit Hilfe von XFDisk die Partitionstabelle geaendert wird, kann es beim
Aufruf des Festplattenmanagers von Windows NT zu folgenden Meldungen kommen:
"Der Festplattenmanager wurde zum ersten mal gestartet...", "Die Partition x
wurde nicht markiert..."
Wenn Sie beide Meldungen mit "Ja" bestaetigen, sichert der Festplattenmanager
die von ihm benoetigten Informationen erneut auf der Festplatte.

Der von Windows NT verwendete Partitionstyp fuer das NTFS Dateisystem ist
"07". Windows NT kann auch auf herkoemmlichen FAT Partitionen installiert
werden. Beachten Sie bitte, dass auf NTFS Laufwerke normalerweise nur von
Windows NT aus zugegriffen werden kann. Spezielle Treiber ermoeglichen einen
Lesezugriff auch von DOS aus (NTFSDOS).
Alle Angaben zu Partitionstypen sind in Hexadezimalschreibweise. Im Anhang A
finden Sie die wichtigsten Partitionstypen aufgelistet.

Wenn Sie standardmaessig NTFS Partitionen anlegen wollen, geben Sie beim
Aufruf von XFDisk den Parameter /WINNT an oder aktivieren Sie den
entsprechenden Eintrag in der Datei XFDISK.INI.


4.4 OS/2
--------

Leider ist OS/2 bei der Installation auf einem logischen Laufwerk auf den
eigenen Boot manager angewiesen. Sie muessen daher in jedem Fall den bei OS/2
mitgelieferten Boot manager installieren, wenn sie OS/2 auf einem logischen
Laufwerk installieren wollen. Nach dem die Installation abgeschlossen ist,
koennen Sie die Boot manager Partition jedoch wieder loeschen und OS/2 ueber
den XFDisk Bootmanager starten. Die Partitionierung erfordert allerdings durch
diesen Trick eine gut durchdachte Planung, damit hinterher keine unnoetigen
Loecher (freie Bereiche) entstehen.

Der von OS/2 verwendete Partitionstyp fuer das HPFS Dateisystem ist "07". OS/2
kann auch auf herkoemmlichen FAT Partitionen installiert werden. Beachten Sie
bitte, dass auf HPFS Laufwerke normalerweise nur von OS/2 aus zugegriffen
werden kann. Spezielle Treiber ermoeglichen den Lesezugriff unter DOS
(HPFSDOS, AMOS) und den Vollzugriff unter Windows NT (PINNBALL.SYS).
Alle Angaben zu Partitionstypen sind in Hexadezimalschreibweise. Im Anhang A
finden Sie die wichtigsten Partitionstypen aufgelistet.

Da es bei den von XFDisk standardmaessig verwendeten Partitionstypen zu
Problemen mit OS/2 kommen kann, geben Sie bitte beim Aufruf von XFDisk den
Parameter /OS2 oder den Parameter /NOWIN9X an oder aktivieren Sie den
entsprechenden Eintrag in der Datei XFDISK.INI.
Da der Parameter /OS2 automatisch den Parameter /NOWIN9X mit auswaehlt,
brauchen Sie nicht beide Parameter einzugeben. Der Parameter /NOWIN9X
unterdrueckt lediglich die Verwendung der neuen Partitionstypen von
Windows 9x. Wenn Sie den Parameter /OS2 verwenden, werden ausserdem neue 
Partitionen mit dem Partitionstyp HPFS angelegt.


4.5 Linux
---------

Linux kann mit dem Bootmanager problemlos gebootet werden, auch wenn es auf
einem logischen Laufwerk installiert wird. Die Partitionierung kann mit dem
XFDisk unter DOS erfolgen oder mit dem Linux eigenen FDISK. Im ersten Fall
muessen Sie lediglich nachtraeglich die Partitionstypen fuer Linux aendern.
Der Linux-eigene Lader LILO muss genauso konfiguriert werden, wie fuer den
OS/2 Boot manager, er darf nicht den MBR veraendern (/dev/hda bzw. /dev/hdb
usw.), sondern nur den Bootcode im root-Verzeichnis (z.B. /hda5). Ein Beispiel
fuer eine lilo.conf finden Sie im Anhang C. Lesen Sie hierzu auch unbedingt
das Handbuch bzw. die Readme Dateien Ihrer Linux-Distribution.

Achtung: Beim Aufruf mit dem Kommandozeilenparameter /SORT speichert XFDisk
         die Partitionstabelleneintraege in der physikalischen Reihenfolge.
         Dadurch koennen sich Eintraege von vorhandenen Partitionen
         verschieben.
         Daher muss gegebenenfalls vor diesen Aktionen die Datei
         /etc/lilo.conf geaendert und LiLo neu installiert werden.

Der von Linux verwendete Partitionstyp fuer das ext2fs Dateisystem ist "83".
Linux kann zwar auch auf herkoemmlichen FAT Partitionen installiert werden
(mit Hilfe des UMSDOS Dateisystems, lesen Sie hierzu die Handbuecher Ihrer
Distribution), dies ist jedoch im Allgemeinen nicht empfehlenswert.
Des weiteren gestatten Linux die Verwendung eine Swappartition, der
entsprechende Partitionstyp ist "82". Normalerweise ist es sinnvoll, Linux
auf ein oder mehrere logische Laufwerke zu verteilen und ein weiteres
logisches Laufwerk fuer die Swappartition anzulegen.
Alle Angaben zu Partitionstypen sind in Hexadezimalschreibweise. Im Anhang A
finden Sie die wichtigsten Partitionstypen aufgelistet.

Wenn Sie standardmaessig ext2fs Partitionen anlegen wollen, geben Sie beim
Aufruf von XFDisk den Parameter /LINUX an oder aktivieren Sie den
entsprechenden Eintrag in der Datei XFDISK.INI.


Anhang
======

A. Partitionstypen und Dateisysteme
-----------------------------------

Alle Kennzahlen der Partitionstypen sind in Hexadezimalschreibweise.

   Typ   Dateisystem
   01    FAT12
   04    FAT16 < 32MB
   06    FAT16 > 32MB
   07    HPFS (OS/2) oder NTFS (Windows NT)
   0A    OS/2 Bootmanager
   0B    FAT32
   0C    FAT32 mit BIOS-Extensions
   0E    FAT16 mit BIOS-Extensions
   0F    erweiterte Partition mit BIOS-Extensions
   82    Linux Swap
   83    Linux Native (ext2fs)

Dies ist nur eine kleine Auswahl der moeglichen Partitionstypen und stellt die
am haeufigsten vorkommenden Typen dar. Wenn das von Ihnen verwendete System
andere Partitionstypen verwendet, lesen Sie bitte im Handbuch des Systems nach
oder Kontaktieren Sie den Hersteller.


B. Beispiel fuer eine DOS/Windows 95 Installation
-------------------------------------------------

Falls Sie vorhaben ihre Festplatte neu einzurichten, oder gerade ihren neuen
Computer einrichten und DOS und Windows 95 separat booten moechten, ist
folgende Vorgehensweise zu empfehlen:

1. Erstellen Sie eine DOS Systemdiskette (ohne Autoexec.bat und Config.sys)
   und kopieren Sie alle Dateien aus dem XFDisk Verzeichnis auf diese
   Diskette.
2. Booten Sie mit der gerade erstellten Diskette und erstellen Sie ein
   primaere Partition (fuer die Windows 95 Bootdateien). Falls Sie von DOS aus
   nur selten oder gar nicht auf die Windows 95 Systemdateien zugreifen
   moechten, so lohnt es sich eine grosse primaere Partition zu erstellen um
   Windows 95 komplett auf eine einzelne Partition zu speichern. So ist es
   z.B. moeglich bei der OSR-2 Version das FAT32 Dateisystem einzusetzen,
   ansonsten planen Sie den Platz fuer weitere Partitionen ein und erstellen
   Sie ein zusaetzliches logisches Laufwerk fuer die Windows 95 Systemdateien.
3. Booten Sie mit der mitgelieferten Windows 95 Bootdiskette und installieren
   Sie Windows 95. Wenn Windows 95 Sie fragt, ob es auch den freien Bereich
   zuordnen soll, antworten Sie mit "Nein".
4. Booten Sie noch mal mit der XFDisk Diskette und erstellen Sie eine weitere
   primaere Partition und alle fuer Sie noetigen logischen Laufwerke. Machen
   Sie die gerade erstellte primaere Partition "sichtbar" und "aktivieren" Sie
   sie gegebenenfalls (lesen Sie hierzu Kapitel 3.2.4).
5. Booten Sie jetzt mit der DOS Installationsdiskette und installieren Sie
   DOS wie gewoehnlich. Alle gerade erstellen logischen Laufwerke sollten Sie
   jetzt von DOS formatieren lassen (DOS fragt automatisch bei Setup danach).
6. Booten Sie zum dritten (und letzten) mal von der XFDisk Bootdiskette und
   installieren Sie den Bootmanager, nehmen Sie die gerade erstellten DOS und
   Windows 95 Partitionen in das Bootmenue auf und bestimmen Sie den Standard-
   Eintrag im Bootmanager Menue. Setzen Sie das gewuenschte Zeitlimit.

Achtung: Hier sei noch einmal explizit darauf hingewiesen, dass DOS und damit
         auch Windows 95 eine primaere Partition auf der ersten Festplatte
         benoetigen, um dort ihre Systemdateien unterzubringen.


C. Beispiel fuer eine lilo.conf
-------------------------------

Die Konfiguration fuer den Linux Bootloader LiLo koennte fuer das
Zusammenspiel mit dem Bootmanager zum Besipiel wie folgt aussehen:

   #LILO Konfigurations-Datei
   #Start LILO global Section
   append="onlycd=none"
   boot=/dev/hda11   #hier sollte die root Partition eingetragen
                     #werden, auf jeden Fall kein /dev/hda, hdb, o.ae.	
   #compact          #faster, but won't work on all systems.
   read-only
   prompt
   timeout=00
   vga = normal	# force sane state
   #End LILO global section
   #Linux bootable partition config begins
   image = /vmlinuz
   root = /dev/hda11
   label = Linux
   #Linux bootable partition config ends

Sonst sollte sonst nichts in der lilo.conf stehen, da lilo nur zum Booten von
Linux verwendet wird


D. Dateien im Originalarchiv
----------------------------

Folgende Dateien muessen sich im Originalarchiv befinden. Sollte eine der
Dateien fehlen, dann bitte ich Sie, mir dies mitzuteilen und mir die Quelle
zu nennen, von der Sie das Archiv erhalten haben. Ausser den hier genannten
Dateien koennen sich auch noch zusaetzliche Dateien in dem Archiv enthalten
sein (z.B. WICHTIG.TXT).

   Datei          Beschreibung/Funktion
   FILE_ID.DIZ    Beschreibung des Archives (fuer Mailboxen)
   HISTORY.TXT    Versionshistorie
   GPL.TXT        GNU General Public License; Lizenzvereinbarung
   README.TXT     User Dokumentation
   XFDISK.EXE     Programmdatei
   XFDISK.XFH     Hilfedatei

Neben dem Programm steht auch der Quellcode oeffentlich zur Verfuegung und
unterliegt den Lizenzbestimmungen der GNU General Public License.


E. Freewarehinweis
------------------

XFDisk wird als FreeWare vom Programmierer weitergegeben. Das heisst, sie
duerfen das Programm beliebig oft kopieren und auf beliebig vielen Computern
zur gleichen Zeit verwenden. Es fallen keine Lizenzgebuehren irgendwelcher Art
an. Sie duerfen des weiteren das Programm an beliebig viele Personen Ihrer
Wahl weitergeben, solange Sie alle Dateien zusammen weitergeben, die sich im
Originalarchiv befinden (siehe auch Anhang E) und solange die Weitergabe auf
das Programm bezogen entgeldfrei erfolgt. Die Weitergabe ist sowohl auf
beliebigen Datentraegermaterialien, als auch in elektronischer Form ueber
Computernetzwerke oder aehnliche Einrichtungen nicht nur erlaubt, sondern
sogar ausdruecklich erwuenscht.

Sollten Sie zur Weitergabe z.B. auf ShareWare CD-ROMs oder aehnlichem eine
"Pauschalvertriebsgenehmigung" benoetigen, dann bitte ich Sie, deswegen mit
mir Kontakt aufzunehmen. Der Absatz ueber die Einschraenkung auf entgeldfreie
Weitergabe bezieht sich nur auf das Programm selber und nicht auf den
Datentraeger, auf dem sich das Programm befindet - ich verlange ja nicht, dass
jemand seine ShareWare CD-ROMs verschenken soll...

Lesen Sie hierzu auch die Datei GPL.TXT.


F. Garantiehinweis
------------------

Die Verwendung von XFDisk erfolgt auf eigene Gefahr!
Es wird seitens des Programmierers weder fuer Datenverluste, Beschaedigungen
von Hard- und/oder Software oder sonstigem, noch fuer evtl. daraus entstehende
Folgeschaeden oder Ausfaelle in irgendeiner Form gehaftet oder Verantwortung
uebernommen.

!!! SICHERN SIE UNBEDINGT IHRE DATEN, BEVOR SIE DIESES PROGRAMM VERWENDEN !!!

XFDisk wird unter Ausschluss jeglicher Garantie und Ansprueche an den
Programmierer weitergegeben und darf nur unter Anerkennung dieses
Ausschlusses und der im vorangegangenen Kapitel beschriebenen Bedingungen
verwendet und weitergegeben werden.

Lesen Sie hierzu auch die Datei GPL.TXT.



G. Eingetragene Warenzeichen
----------------------------

Die meisten der hier verwendeten Namen und Bezeichnungen im Zusammenhang mit
Betriebssystemen, Dateisystemen, etc. sind eingetragene Warenzeichen der
jeweiligen Inhaber.


H. Danksagungen
---------------

Besonderer Dank gebuehrt

   Peter Fischer-Haaser   Fuer neue Ideen zu Programm und Vertrieb
                          Fuer die aktive Mitgestaltung des Programmes
                          Fuer die Pflege der Windows Helpfiles
   Ulrich Mueller         Fuer die Implementierung der INT 13 Extensions
                          Fuer die Weiterentwicklung von XFDisk
   Bernd Troszynski       Fuer die aktive Mitgestaltung des Programmes
   Anton Tokar            Fuer Teile der User Dokumentation
   Helmut Grams           Fuer die Niederlaendische Version

Dank geht an all meine Betatester

   Frank Hennig
   Peter Fischer-Haaser
   Rudi Grzybeck
   Mark Junker
   Christian Klak
   Sven Leutloff
   Anton Tokar
   Bernd Troszynski
   Sebastian Schuberth
   Daniel Voelker
   Maik Wieland
   und alle anderen...

Danken moechte ich auch

   Ralph Brown         Fuer die Interrupt List
   Peter Monadjemi     Fuer eine solide Einfuehrung in Assembler
   Thomas Moenkemeier  Fuer die sehr informative FAQ zum Thema Shareware
   Michael Tischer     Fuer seine sehr hilfreichen Hardwarestudien


==============================================================================
Copyright (C) 1995-99 by F. Painke und A. Tokar. All Rights reserved.
aktualisiert fuer Version 0.9.x von U. Mueller
