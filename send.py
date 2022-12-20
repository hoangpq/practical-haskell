import socket as s

sock = s.socket(s.AF_UNIX)
sock.connect('/tmp/a.sock')

data = "Hello Server!";
sock.send(data.encode());
