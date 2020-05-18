package it.cnr.istc.pst.exploraa.server;

import java.io.IOException;

import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;

import com.fasterxml.jackson.databind.ObjectMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;;

/**
 * ExPLoRAA Server
 */
public class App {

    static final Logger LOG = LoggerFactory.getLogger(App.class);
    static final EntityManagerFactory EMF = Persistence.createEntityManagerFactory("ExPLoRAA_PU");
    static final ObjectMapper MAPPER = new ObjectMapper();

    public static void main(final String[] args) throws IOException {
        RESTService.getInstance().start();
        MessagingService.getInstance().start();

        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            RESTService.getInstance().stop();
            MessagingService.getInstance().stop();
            EMF.close();
        }));
    }
}