package hello.micronaut;

import com.ingdirect.es.core.dao.dto.DTO;
import com.ingdirect.es.mus.F2eEvent;

/**
 * Created by nb38tv on 20/02/2017.
 */
public class SpringBootEntityDaoDTO extends DTO  {

    private Integer entityId;

    private String title;
    private String description;

    public SpringBootEntityDaoDTO() {
        this( defaultEvent());
    }

    public SpringBootEntityDaoDTO(F2eEvent event) {
        super(event);
    }

    public void setEntityId(Integer entityId) {
        this.entityId = entityId;
    }

    public Integer getEntityId() {
        return entityId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    private static F2eEvent defaultEvent() {
        return new F2eEvent() {

            @Override
            public int getPriority() {
                return 0;
            }

            @Override
            public String getId() {
                return "0";
            }
        };
    }


}