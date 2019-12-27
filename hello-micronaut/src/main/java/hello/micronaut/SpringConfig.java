package hello.micronaut;

import org.glassfish.jersey.server.ResourceConfig;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SpringConfig extends ResourceConfig {

    private static final String RESOURCE_PACKAGE = "com.ingbank.f2e.presentation.resource";

    public SpringConfig() {
        packages(RESOURCE_PACKAGE);
    }

}
