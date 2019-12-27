package hello.micronaut;

import com.ingdirect.es.connector.restTpa.RequestInfoDTO;
import com.ingdirect.es.core.mus.dto.MethodDTO;
import com.ingdirect.es.core.service.connectorManager.executors.impl.ReactorConnectorManager;
import com.ingdirect.es.mus.F2eEvent;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import reactor.core.publisher.Flux;

import javax.inject.Inject;

@Controller("/hello")
public class HelloController {

    @Inject
    private RetryStrategy retryStrategy;

    @Inject
    private ReactorConnectorManager<RequestInfoDTO, SpringBootEntityDaoDTO> reactorConnectorRestManagerExecutor;

    @Get(produces = MediaType.TEXT_PLAIN)
    public Flux<SpringBootEntityDaoDTO> index() {

        RequestInfoDTO requestInfoDTO =
                RequestInfoDTO.withF2eEvent(getF2eEvent())
                        .withMethod(MethodDTO.READ)
                        .withResponseClass(SpringBootEntityDaoDTO.class)
                        .withUri("localhost:8500")
                        .withPath("/home/payload");

        return reactorConnectorRestManagerExecutor.execute("rest", requestInfoDTO, MethodDTO.READ);
    }

    private F2eEvent getF2eEvent() {
        return new F2eEvent() {
            @Override
            public String getId() {
                return "id";
            }

            @Override
            public int getPriority() {
                return 0;
            }
        };
    }
}