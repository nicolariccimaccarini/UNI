package remote_dgram_strlen_java;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

public class clientDgramRstrlen {
    public static void main(String[] args) {
        try {
            BufferedReader userIn = new BufferedReader(new InputStreamReader(System.in));
            System.out.println("\nInserire stringa: ");
            String stringa = userIn.readLine();

            InetAddress serverAddr = InetAddress.getByName(args[0]);
            int porta              = Integer.parseInt(args[1]);

            DatagramSocket cs = new DatagramSocket();

            // invio
            byte[] request       = stringa.getBytes("UTF-8");
            DatagramPacket pkOut = new DatagramPacket(request, request.length, serverAddr, porta);

            System.out.println("\nInvio a server: " + stringa);
            cs.send(pkOut);

            // ricezione
            byte[] buf              = new byte[2048];
            DatagramPacket response = new DatagramPacket(buf, buf.length);

            cs.receive(response);

            String risposta = new String(response.getData(), "UTF-8");
            risposta        = risposta.trim();
            System.out.println("Server ha risposto: " + risposta);

            cs.close();
        } catch (IOException e) {
            /*TODO*/
        }
    }
}
