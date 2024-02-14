package cn.wubo.sql.util.web;

import cn.wubo.sql.util.exception.EntityWebException;
import cn.wubo.sql.util.result.Result;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.function.RouterFunction;
import org.springframework.web.servlet.function.RouterFunctions;
import org.springframework.web.servlet.function.ServerResponse;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

@Configuration
public class EntityWebConfig {

    @Bean
    public EntityWebService entityWebService() {
        return new EntityWebService();
    }

    @Bean("wb04307201SqlWebRouter")
    public RouterFunction<ServerResponse> entityWebRouter(EntityWebService entityWebService) {
        return RouterFunctions.route().GET("/entity/view/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> map = new HashMap<>();
            map.put("contextPath", request.requestPath().contextPath().value());
            map.put("id", id);
            map.put("data", entityWebService.view(id));
            return ServerResponse.ok().contentType(MediaType.TEXT_HTML).body(write("table.ftl", map));
        }).POST("/entity/select/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> params = request.body(new ParameterizedTypeReference<Map<String, Object>>() {
            });
            return ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).body(Result.success(entityWebService.select(id, params)));
        }).POST("/entity/save/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> params = request.body(new ParameterizedTypeReference<Map<String, Object>>() {
            });
            return ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).body(Result.success(entityWebService.save(id, params)));
        }).POST("/entity/delete/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> params = request.body(new ParameterizedTypeReference<Map<String, Object>>() {
            });
            return ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).body(Result.success(entityWebService.delete(id, params)));
        }).POST("/entity/getById/{id}", request -> {
            String id = request.pathVariable("id");
            Map<String, Object> params = request.body(new ParameterizedTypeReference<Map<String, Object>>() {
            });
            return ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).body(Result.success(entityWebService.getById(id, params)));
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
            throw new EntityWebException(e.getMessage(), e);
        }
    }
}
