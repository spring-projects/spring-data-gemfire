package org.springframework.data.gemfire.config.annotation;

import org.springframework.data.gemfire.config.annotation.support.Configurer;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

public interface GatewaySenderConfigurer extends Configurer<GatewaySenderFactoryBean> {

	/**
	 * Configuration callback method providing a reference to a {@link org.springframework.data.gemfire.wan.GatewaySenderFactoryBean} used to construct,
	 * configure and initialize an instance of {@link org.apache.geode.cache.wan.GatewaySender}.
	 *
	 * @param beanName name of the {@link org.apache.geode.cache.wan.GatewaySender} bean declared in the Spring application context.
	 * @param bean reference to the {@link GatewaySenderFactoryBean}.
	 * @see org.springframework.data.gemfire.wan.GatewaySenderFactoryBean
	 * @see org.apache.geode.cache.wan.GatewaySender
	 */
	void configure(String beanName, GatewaySenderFactoryBean bean);
}
