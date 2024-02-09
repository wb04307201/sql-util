package cn.wubo.sql.util.web;

import freemarker.template.Template;
import freemarker.template.TemplateException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.function.RouterFunction;
import org.springframework.web.servlet.function.RouterFunctions;
import org.springframework.web.servlet.function.ServerResponse;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

@Configuration
public class SqlWebConfig {

    @Bean
    public SqlWebService sqlWebService() {
        return new SqlWebService();
    }

    @Bean("wb04307201SqlWebRouter")
    public RouterFunction<ServerResponse> sqlWebRouter(SqlWebService sqlWebService) {
        return RouterFunctions.route().GET("/jgradio/view/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> map = new HashMap<>();
            map.put("contextPath", request.requestPath().contextPath().value());
            map.put("id", id);
            //map.put("data", JSONObject.from(sqlWebService.view(id)));
            return ServerResponse.ok().contentType(MediaType.TEXT_HTML).body(write("table.ftl", map));
        }).POST("/jgradio/select/{id}", request -> {
            String id = request.pathVariable("id");
            //JGradioQuery query = request.body(JGradioQuery.class);
            return null;
        }).build();
    }

    private String write(String templateName, Map<String, Object> params) {
        try (StringWriter sw = new StringWriter()) {
            freemarker.template.Configuration cfg = new freemarker.template.Configuration(freemarker.template.Configuration.VERSION_2_3_23);
            cfg.setClassForTemplateLoading(this.getClass(), "/template");
            Template template = cfg.getTemplate(templateName, "UTF-8");
            template.process(params, sw);
            return sw.toString();
        } catch (TemplateException | IOException e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }
}
