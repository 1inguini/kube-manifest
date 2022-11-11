INSTALL SONAME 'auth_ed25519';
DROP USER IF EXISTS '' @localhost;
DROP USER IF EXISTS '' @buildkitsandbox;
DROP USER IF EXISTS nonroot @localhost;
DROP USER IF EXISTS root @localhost;
GRANT ALL PRIVILEGES ON *.* TO 'root' @'%' IDENTIFIED VIA ed25519 USING PASSWORD('secret');