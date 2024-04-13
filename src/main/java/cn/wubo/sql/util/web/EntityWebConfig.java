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

    /**
     * 配置实体相关的Web路由。
     * 这个方法定义了如何处理与实体相关的HTTP请求，包括获取实体详情、根据条件选择实体、保存实体、删除实体以及根据ID获取实体。
     *
     * @param entityWebService 用于处理实体服务请求的Web服务。这个参数是一个实现了特定服务逻辑的Bean，将被用于实际处理HTTP请求中的业务逻辑。
     * @return RouterFunction<ServerResponse> 路由功能对象。这个对象定义了一系列的路由规则，用于处理不同的HTTP请求并返回响应。
     */
    @Bean("wb04307201SqlWebRouter")
    public RouterFunction<ServerResponse> entityWebRouter(EntityWebService entityWebService) {
        // 构建路由功能对象，通过链式调用定义不同路径和方法的处理逻辑
        return RouterFunctions.route().GET("/entity/view/{id}", request -> {
            // 处理GET请求，获取指定ID的实体详情，并返回HTML视图
            String id = request.pathVariable("id"); // 从URL中获取ID参数
            Map<String, Object> map = new HashMap<>(); // 用于存储视图中需要的数据
            map.put("contextPath", request.requestPath().contextPath().value()); // 添加上下文路径到map
            map.put("id", id); // 添加ID到map
            map.put("data", entityWebService.view(id)); // 调用服务逻辑获取实体详情，并添加到map
            // 返回构建好的HTML响应
            return ServerResponse.ok().contentType(MediaType.TEXT_HTML).body(write("table.ftl", map));
        }).POST("/entity/select/{id}", request -> {
            // 处理POST请求，根据条件选择实体，并返回JSON格式的结果
            String id = request.pathVariable("id"); // 从URL中获取ID参数
            Map<String, Object> params = request.body(new ParameterizedTypeReference<>() {
            }); // 从请求体中读取参数
            // 返回调用服务逻辑后的选择结果
            return ServerResponse.ok().contentType(MediaType.APPLICATION_JSON).body(Result.success(entityWebService.select(id, params)));
        })// 后续路径和方法的处理逻辑类似，主要区别在于调用的服务方法和返回的数据格式
        .build();
    }

    /**
     * 使用FreeMarker模板引擎，根据模板名称和参数生成HTML字符串。
     *
     * @param templateName 模板的名称，指定在模板目录下要使用的模板文件。
     * @param params 要在模板中使用的参数，是一个键值对映射。
     * @return 根据给定的模板和参数生成的HTML字符串。
     * @throws EntityWebException 如果模板处理过程中发生异常，则抛出此异常。
     */
    private String write(String templateName, Map<String, Object> params) {
        try (StringWriter sw = new StringWriter()) {
            // 初始化FreeMarker配置，设置模板加载路径
            freemarker.template.Configuration cfg = new freemarker.template.Configuration(freemarker.template.Configuration.VERSION_2_3_23);
            cfg.setClassForTemplateLoading(this.getClass(), "/template");

            // 加载指定名称和编码的模板
            Template template = cfg.getTemplate(templateName, "UTF-8");

            // 使用模板和参数生成输出，写入到StringWriter中
            template.process(params, sw);

            return sw.toString();
        } catch (TemplateException | IOException e) {
            // 处理模板处理过程中的异常，转换并抛出为EntityWebException
            throw new EntityWebException(e.getMessage(), e);
        }
    }
}
