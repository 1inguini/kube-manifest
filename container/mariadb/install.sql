DROP USER IF EXISTS '' @localhost;
DROP USER IF EXISTS '' @buildkitsandbox;
DROP DATABASE IF EXISTS test;
INSTALL SONAME 'auth_ed25519';
-- ALTER USER 'root' @'localhost' IDENTIFIED VIA unix_socket;