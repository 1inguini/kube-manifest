INSTALL SONAME 'auth_ed25519';
DROP USER IF EXISTS '' @localhost;
DROP USER IF EXISTS '' @buildkitsandbox;
DROP USER IF EXISTS nonroot @localhost;
GRANT ALL PRIVILEGES ON *.* TO 'root' @'localhost' IDENTIFIED VIA ed25519 USING PASSWORD('secret');