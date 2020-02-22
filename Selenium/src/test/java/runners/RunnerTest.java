package runners;

import baseclasses.BaseRunner;
import configuration.ConfigurationFile;
import cucumber.api.CucumberOptions;
import cucumber.api.junit.Cucumber;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;


@RunWith(Cucumber.class)
@CucumberOptions(
        features = {"src/test/resources/features"},
        glue = {"steps","baseclasses"},
        plugin = {"json:target/cucumber.json"},
        tags={"@userprofile or @authorization"}
//        tags={"@userprofile"}
//        tags={"@debug"}
)

@ConfigurationFile("firefox.properties")
public class RunnerTest extends BaseRunner {

    @BeforeClass
    public static void beforeTests() {
        beforeScenarios((RunnerTest.class).getAnnotation(ConfigurationFile.class));
    }

    @AfterClass
    public static void afterTests() {
        afterScenarios();
    }

}
