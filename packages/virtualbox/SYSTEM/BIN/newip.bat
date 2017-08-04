:: This batch script is designed to re-obtain an IP address after system startup.
@ECHO OFF
LH \SYSTEM\BIN\DHCP -retries 10 -timeout 20
SET MTCPCFG=\SYSTEM\NET\MTCPCFG.TXT