package runners;

import baseclasses.BaseRunner;
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
        tags={"~@authorization", "@localize"}
)

public class RunnerTest extends BaseRunner {

    @BeforeClass
    public static void beforeClass() {
        beforeScenarios("chrome", "c:/selenium/chromedriver.exe", 10);
//        beforeScenarios("firefox", "c:/selenium/geckodriver.exe", 10);
//        beforeScenarios("ie", "c:/selenium/geckodriver.exe", 10);
    }

    @AfterClass
    public static void afterClass() {
        afterScenarios();
    }

}
