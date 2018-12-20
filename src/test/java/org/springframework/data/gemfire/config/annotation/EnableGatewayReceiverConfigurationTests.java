package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.Order;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;
import org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiver;
import org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiverConfiguration;
import org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiverConfigurer;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.*;

/**
 * Tests for {@link org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiver}.
 *
 * @author Udo Kohlmeyer
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 * @see EnableGatewayReceiverConfigurer
 * @see EnableGatewayReceiverConfiguration
 * @since 2.2.0
 */
public class EnableGatewayReceiverConfigurationTests {

    private ConfigurableApplicationContext applicationContext;

    @After
    public void shutdown()
    {
        Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
    }

    @Configuration
    @CacheServerApplication
    @EnableGatewayReceiver(manualStart = false, startPort = 10000, endPort = 11000, maximumTimeBetweenPings = 1000,
            socketBufferSize = 16384, bindAddress = "localhost",transportFilters = {"transportBean1", "transportBean2"},
            hostnameForSenders = "hostnameLocalhost")
    static class TestConfigurationWithOrder {

        @Bean("transportBean1")
        @Order(1)
        GatewayTransportFilter createGatewayTransportBean1() {
            return new TestGatewayTransportFilter("transportBean1");
        }

        @Bean("transportBean2")
        @Order(2)
        GatewayTransportFilter createGatewayTransportBean2() {
            return new TestGatewayTransportFilter("transportBean2");
        }

        @Bean("gatewayConfigurer")
		EnableGatewayReceiverConfigurer gatewayReceiverConfigurer() {
            return new TestGatewayReceiverConfigurer();
        }
    }

    @Configuration
    @CacheServerApplication
    @EnableGatewayReceiver(manualStart = false, startPort = 12000, endPort = 13000, maximumTimeBetweenPings = 5000,
            socketBufferSize = 32768, transportFilters = {"transportBean1", "transportBean2"}, bindAddress = "localhost",
            hostnameForSenders = "hostnameLocalhost")
    static class TestConfigurationFromAnnotation {

        @Bean("transportBean1")
        GatewayTransportFilter createGatewayTransportBean1() {
            return new TestGatewayTransportFilter("transportBean1");
        }

        @Bean("transportBean2")
        GatewayTransportFilter createGatewayTransportBean2() {
            return new TestGatewayTransportFilter("transportBean2");
        }

        @Bean("gatewayConfigurer")
		EnableGatewayReceiverConfigurer gatewayReceiverConfigurer() {
            return new TestGatewayReceiverConfigurer();
        }
    }

    @Test
    public void annotationConfiguredGatewayTransportFiltersOrdered() {
        this.applicationContext = newApplicationContext(TestConfigurationFromAnnotation.class);
        TestGatewayReceiverConfigurer gatewayReceiverConfigurer = (TestGatewayReceiverConfigurer) this.applicationContext.getBean(
			EnableGatewayReceiverConfigurer.class);

        GatewayReceiver gatewayReceiver = this.applicationContext.getBean("GatewayReceiver",GatewayReceiver.class);

        assertThat(gatewayReceiver.getStartPort()).isEqualTo(12000);
        assertThat(gatewayReceiver.getEndPort()).isEqualTo(13000);
        assertThat(gatewayReceiver.getBindAddress()).isEqualTo("localhost");
        assertThat(gatewayReceiver.getHostnameForSenders()).isEqualTo("hostnameLocalhost");
        assertThat(gatewayReceiver.getMaximumTimeBetweenPings()).isEqualTo(5000);
        assertThat(gatewayReceiver.getSocketBufferSize()).isEqualTo(32768);
        assertThat(gatewayReceiver.isManualStart()).isEqualTo(false);
        assertThat(gatewayReceiver.getGatewayTransportFilters().size()).isEqualTo(2);
        assertThat(((EnableGatewayReceiverConfigurationTests.TestGatewayTransportFilter)gatewayReceiver.getGatewayTransportFilters().get(0)).name).isEqualTo("transportBean1");
        assertThat(((EnableGatewayReceiverConfigurationTests.TestGatewayTransportFilter)gatewayReceiver.getGatewayTransportFilters().get(1)).name).isEqualTo("transportBean2");
        assertThat(gatewayReceiverConfigurer.beanNames.toArray()).isEqualTo(new String[]{"transportBean1", "transportBean2"});
    }

    @Test
    public void beanConfiguredGatewayTransportFiltersOrdered() {
        this.applicationContext = newApplicationContext(TestConfigurationWithOrder.class);
        TestGatewayReceiverConfigurer gatewayReceiverConfigurer = (TestGatewayReceiverConfigurer) this.applicationContext.getBean(
			EnableGatewayReceiverConfigurer.class);

        GatewayReceiver gatewayReceiver = this.applicationContext.getBean("GatewayReceiver",GatewayReceiver.class);

        assertThat(gatewayReceiver.getStartPort()).isEqualTo(10000);
        assertThat(gatewayReceiver.getEndPort()).isEqualTo(11000);
        assertThat(gatewayReceiver.getBindAddress()).isEqualTo("localhost");
        assertThat(gatewayReceiver.getHostnameForSenders()).isEqualTo("hostnameLocalhost");
        assertThat(gatewayReceiver.getMaximumTimeBetweenPings()).isEqualTo(1000);
        assertThat(gatewayReceiver.getSocketBufferSize()).isEqualTo(16384);
        assertThat(gatewayReceiver.isManualStart()).isEqualTo(false);
        assertThat(gatewayReceiver.getGatewayTransportFilters().size()).isEqualTo(2);
        assertThat(((EnableGatewayReceiverConfigurationTests.TestGatewayTransportFilter)gatewayReceiver.getGatewayTransportFilters().get(0)).name).isEqualTo("transportBean1");
        assertThat(((EnableGatewayReceiverConfigurationTests.TestGatewayTransportFilter)gatewayReceiver.getGatewayTransportFilters().get(1)).name).isEqualTo("transportBean2");
        assertThat(gatewayReceiverConfigurer.beanNames.toArray()).isEqualTo(new String[]{"transportBean1", "transportBean2"});
    }

    private static class TestGatewayReceiverConfigurer implements EnableGatewayReceiverConfigurer, Iterable<String> {

        private final List<String> beanNames = new ArrayList<>();

        @Override
        public Iterator<String> iterator() {
            return Collections.unmodifiableList(this.beanNames).iterator();
        }

        @Override
        public void configure(String beanName, GatewayReceiverFactoryBean bean) {
            bean.getTransportFilters().stream().forEach(o -> beanNames.add(((TestGatewayTransportFilter) o).name));
        }
    }

    private static class TestGatewayTransportFilter implements GatewayTransportFilter {
        private String name;

        public TestGatewayTransportFilter(String name) {
            this.name = name;
        }

        @Override
        public InputStream getInputStream(InputStream inputStream) {
            return null;
        }

        @Override
        public OutputStream getOutputStream(OutputStream outputStream) {
            return null;
        }
    }

    private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

        ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);

        applicationContext.registerShutdownHook();

        return applicationContext;
    }
}
