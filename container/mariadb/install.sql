-- INSTALL SONAME 'auth_ed25519';
-- GRANT ALL PRIVILEGES ON *.* TO 'root' @'localhost' IDENTIFIED VIA ed25519 USING PASSWORD('secret');
-- DROP USER IF EXISTS '' @localhost;
DROP USER IF EXISTS '' @buildkitsandbox;