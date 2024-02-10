package cn.wubo.sql.util.test;

import cn.wubo.sql.util.ModelSqlUtils;
import cn.wubo.sql.util.MutilConnectionPool;
import cn.wubo.sql.util.SQL;
import cn.wubo.sql.util.entity.EntityUtils;
import freemarker.template.Template;
import freemarker.template.TemplateException;

import java.io.IOException;
import java.io.StringWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TestController {

    public static void main(String[] args) {
        try (StringWriter sw = new StringWriter()) {
            Map<String, Object> map = new HashMap<>();
            map.put("contextPath", "11111");
            map.put("id", "11111");
            map.put("data", EntityUtils.getTable(User.class));

            freemarker.template.Configuration cfg = new freemarker.template.Configuration(freemarker.template.Configuration.VERSION_2_3_23);
            cfg.setClassForTemplateLoading(User.class, "/template");
            Template template = cfg.getTemplate("table.ftl", "UTF-8");
            template.process(map, sw);
            System.out.println(sw);
        } catch (TemplateException | IOException e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }
}
