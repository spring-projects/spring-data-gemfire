package org.springframework.data.gemfire.wan.annotation;

import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;

public interface EnableGatewayReceiverConfigurer {
    /**
     * Configuration callback method providing a reference to a {@link GatewayReceiverFactoryBean} used to construct,
     * configure and initialize an instance of {@link org.apache.geode.cache.wan.GatewayReceiver}.
     *
     * @param beanName name of the {@link org.apache.geode.cache.wan.GatewayReceiver} bean declared in the Spring application context.
     * @param bean reference to the {@link GatewayReceiverFactoryBean}.
     * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
     * @see org.apache.geode.cache.wan.GatewayReceiver
     */
    void configure(String beanName, GatewayReceiverFactoryBean bean);
}
