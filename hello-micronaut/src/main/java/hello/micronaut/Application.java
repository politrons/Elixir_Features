package hello.micronaut;

import com.ing.core.connector.bootable.ReactiveConnectorBootable;
import io.micronaut.runtime.Micronaut;
import io.micronaut.spring.context.MicronautApplicationContext;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;


@Configuration
@SpringBootApplication(exclude = {org.springframework.boot.autoconfigure.cassandra.CassandraAutoConfiguration.class,
        org.springframework.boot.autoconfigure.amqp.RabbitAutoConfiguration.class,
        org.springframework.boot.autoconfigure.mongo.MongoAutoConfiguration.class,
        org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration.class})
@ComponentScan(basePackageClasses = HelloController.class)
public class Application {

    public static ConfigurableApplicationContext springContext;

    public static void main(String[] args) throws Exception {

        System.setProperty("akka.config", "/Users/nb38tv/workspace/hello-micronaut/src/main/resources/config/");
        System.setProperty("akka.config.file", "application.conf");
        springContext = ReactiveConnectorBootable.loadReactiveConnector();

//        ConfigurableApplicationContext context = new MicronautApplicationContext();
//        context.setParent(springContext);
//        context.refresh();
//        context.start();

        Micronaut.run(Application.class);

    }



}