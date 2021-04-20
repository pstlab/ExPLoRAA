package it.cnr.istc.pst.exploraa;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WCB {

    private static final Logger LOG = LoggerFactory.getLogger(WCB.class);
    private final Client client = ClientBuilder.newClient();
    private final WebTarget target;

    public WCB() {
        final String server_uri = "http://" + App.PROPERTIES.getProperty("wcb_host", "localhost") + ':'
                + App.PROPERTIES.getProperty("wcb_port", "5013");
        target = client.target(server_uri);
    }

    public JsonNode wiki(final String page, final String mode) throws JsonProcessingException {
        LOG.info("analyzing page {}", page);
        Response response = target.path("wiki").queryParam("page", page).queryParam("m", mode)
                .request(MediaType.APPLICATION_JSON).get();
        return App.MAPPER.readTree(response.readEntity(String.class));
    }
}
