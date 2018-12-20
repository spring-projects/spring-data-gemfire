package org.springframework.data.gemfire.wan.annotation;

import org.apache.geode.cache.wan.GatewayReceiver;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.factory.config.BeanReference;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Optional;

/**
 * Spring {@link Configuration} class used to construct, configure and initialize a {@link GatewayReceiver} instance
 * in a Spring application context.
 *
 * @author Udo Kohlmeyer
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiver
 * @since 2.2.0
 */
public class EnableGatewayReceiverConfiguration extends AbstractAnnotationConfigSupport
	implements ImportBeanDefinitionRegistrar {

	static final boolean DEFAULT_MANUAL_START = GatewayReceiver.DEFAULT_MANUAL_START;
	static final int DEFAULT_START_PORT = GatewayReceiver.DEFAULT_START_PORT;
	static final int DEFAULT_END_PORT = GatewayReceiver.DEFAULT_END_PORT;
	static final int DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS = GatewayReceiver.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;
	static final int DEFAULT_SOCKET_BUFFER_SIZE = GatewayReceiver.DEFAULT_SOCKET_BUFFER_SIZE;
	static final String DEFAULT_BIND_ADDRESS = GatewayReceiver.DEFAULT_BIND_ADDRESS;
	static final String DEFAULT_HOSTNAME_FOR_SENDERS = GatewayReceiver.DEFAULT_HOSTNAME_FOR_SENDERS;
	private final String START_PORT_LITERAL = "startPort";
	private final String END_PORT_LITERAL = "END_PORT_LITERAL";
	private final String MANUAL_START_LITERAL = "manualStart";
	private final String MAXIMUM_TIME_BETWEEN_PINGS_LITERAL = "maximumTimeBetweenPings";
	private final String SOCKET_BUFFER_SIZE_LITERAL = "socketBufferSize";
	private final String BIND_ADDRESS_LITERAL = "bindAddress";
	private final String HOSTNAME_FOR_SENDERS_LITERAL = "hostnameForSenders";
	private final String TRANSPORT_FILTERS_LITERAL = "transportFilters";

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return org.springframework.data.gemfire.wan.annotation.EnableGatewayReceiver.class;
	}

	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		if (importingClassMetadata.hasAnnotation(getAnnotationTypeName())) {

			AnnotationAttributes enableGatewayReceiverAttributes =
				AnnotationAttributes.fromMap(importingClassMetadata.getAnnotationAttributes(getAnnotationTypeName()));

			registerGatewayReceiverBeanDefinition(enableGatewayReceiverAttributes, registry);
		}
	}

	/**
	 * Configures a {@link GatewayReceiver} from the {@link EnableGatewayReceiver} annotation, "spring.data.gemfire.gateway.receiver.*"
	 * properties and {@link EnableGatewayReceiverConfigurer}
	 *
	 * @param enableGatewayReceiverAttributes
	 * @param registry
	 */
	private void registerGatewayReceiverBeanDefinition(AnnotationAttributes enableGatewayReceiverAttributes,
		BeanDefinitionRegistry registry) {
		BeanDefinitionBuilder gatewayReceiverBeanBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(GatewayReceiverFactoryBean.class);

		String gatewayReceiverBeanName = "GatewayReceiver";

		configureBeanFromAnnotatedProperties(enableGatewayReceiverAttributes, gatewayReceiverBeanBuilder,
			gatewayReceiverBeanName);

		configureBeanBuilderFromPropertiesOrWithDefaultValues(gatewayReceiverBeanBuilder);

		registerGatewayTransportFilterDependencies(enableGatewayReceiverAttributes, gatewayReceiverBeanBuilder);

		registry.registerBeanDefinition(gatewayReceiverBeanName, gatewayReceiverBeanBuilder.getBeanDefinition());
	}

	/**
	 * Configures GatewayReceiver {@link BeanDefinitionBuilder} using value populated on {@link EnableGatewayReceiver}.
	 * If no values are provided, values are set to defaults
	 *
	 * @param enableGatewayReceiverAttributes
	 * @param gatewayReceiverBeanBuilder
	 * @param gatewayReceiverBeanName
	 */
	private void configureBeanFromAnnotatedProperties(AnnotationAttributes enableGatewayReceiverAttributes,
		BeanDefinitionBuilder gatewayReceiverBeanBuilder, String gatewayReceiverBeanName) {
		gatewayReceiverBeanBuilder.addConstructorArgReference(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		gatewayReceiverBeanBuilder.addPropertyValue("beanName", gatewayReceiverBeanName);

		gatewayReceiverBeanBuilder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, START_PORT_LITERAL,
			enableGatewayReceiverAttributes.<Integer>getNumber(START_PORT_LITERAL), DEFAULT_START_PORT);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, END_PORT_LITERAL,
			enableGatewayReceiverAttributes.<Integer>getNumber(END_PORT_LITERAL), DEFAULT_END_PORT);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, MANUAL_START_LITERAL,
			enableGatewayReceiverAttributes.getBoolean(MANUAL_START_LITERAL), DEFAULT_MANUAL_START);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, MAXIMUM_TIME_BETWEEN_PINGS_LITERAL,
			enableGatewayReceiverAttributes.<Integer>getNumber(MAXIMUM_TIME_BETWEEN_PINGS_LITERAL),
			DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, SOCKET_BUFFER_SIZE_LITERAL,
			enableGatewayReceiverAttributes.<Integer>getNumber(SOCKET_BUFFER_SIZE_LITERAL), DEFAULT_SOCKET_BUFFER_SIZE);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, BIND_ADDRESS_LITERAL,
			enableGatewayReceiverAttributes.getString(BIND_ADDRESS_LITERAL), DEFAULT_BIND_ADDRESS);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, HOSTNAME_FOR_SENDERS_LITERAL,
			enableGatewayReceiverAttributes.getString(HOSTNAME_FOR_SENDERS_LITERAL), DEFAULT_HOSTNAME_FOR_SENDERS);

		setPropertyValueIfNotDefault(gatewayReceiverBeanBuilder, TRANSPORT_FILTERS_LITERAL,
			resolveTransportBeanReferences(enableGatewayReceiverAttributes.getStringArray(TRANSPORT_FILTERS_LITERAL)),
			new ManagedList<BeanReference>());
	}

	/**
	 * Configures GatewayReceiver {@link BeanDefinitionBuilder} using properties, defined under "spring.data.gemfire.gateway.receiver.*"
	 *
	 * @param gatewayReceiverBeanBuilder
	 */
	private void configureBeanBuilderFromPropertiesOrWithDefaultValues(
		BeanDefinitionBuilder gatewayReceiverBeanBuilder) {
		MutablePropertyValues beanPropertyValues = gatewayReceiverBeanBuilder.getRawBeanDefinition()
			.getPropertyValues();
		configureFromProperties(gatewayReceiverBeanBuilder, START_PORT_LITERAL,
			(Integer) beanPropertyValues.getPropertyValue(START_PORT_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, END_PORT_LITERAL,
			(Integer) beanPropertyValues.getPropertyValue(END_PORT_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, MANUAL_START_LITERAL,
			(Boolean) beanPropertyValues.getPropertyValue(MANUAL_START_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, MAXIMUM_TIME_BETWEEN_PINGS_LITERAL,
			(Integer) beanPropertyValues.getPropertyValue(MAXIMUM_TIME_BETWEEN_PINGS_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, SOCKET_BUFFER_SIZE_LITERAL,
			(Integer) beanPropertyValues.getPropertyValue(SOCKET_BUFFER_SIZE_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, BIND_ADDRESS_LITERAL,
			(String) beanPropertyValues.getPropertyValue(BIND_ADDRESS_LITERAL).getValue());
		configureFromProperties(gatewayReceiverBeanBuilder, HOSTNAME_FOR_SENDERS_LITERAL,
			(String) beanPropertyValues.getPropertyValue(HOSTNAME_FOR_SENDERS_LITERAL).getValue());

		String[] filters = resolveProperty(gatewayReceiverProperty(TRANSPORT_FILTERS_LITERAL), String[].class);
		Optional.ofNullable(filters)
			.ifPresent(transportFilters ->
			{
				ManagedList<BeanReference> beanReferences = resolveTransportBeanReferences(transportFilters);
				gatewayReceiverBeanBuilder.addPropertyValue(TRANSPORT_FILTERS_LITERAL, beanReferences);
			});
	}

	private void configureFromProperties(BeanDefinitionBuilder gatewayReceiverBeanBuilder, String propertyName,
		Integer annotatedPropertyValue) {
		Integer propertyValue = resolveProperty(gatewayReceiverProperty(propertyName), annotatedPropertyValue);
		gatewayReceiverBeanBuilder.addPropertyValue(propertyName, propertyValue);
	}


	private void configureFromProperties(BeanDefinitionBuilder gatewayReceiverBeanBuilder, String propertyName,
		String annotatedPropertyValue) {
		String propertyValue = resolveProperty(gatewayReceiverProperty(propertyName), annotatedPropertyValue);
		gatewayReceiverBeanBuilder.addPropertyValue(propertyName, propertyValue);
	}

	private void configureFromProperties(BeanDefinitionBuilder gatewayReceiverBeanBuilder, String propertyName,
		Boolean annotatedPropertyValue) {
		Boolean propertyValue = resolveProperty(gatewayReceiverProperty(propertyName), annotatedPropertyValue);
		gatewayReceiverBeanBuilder.addPropertyValue(propertyName, propertyValue);
	}

	private ManagedList<BeanReference> resolveTransportBeanReferences(String[] transportFilters) {
		ManagedList<BeanReference> transportFilterBeanReferences = new ManagedList<>();

		Optional.ofNullable(transportFilters).ifPresent(transportFilterBeanNames ->
			Arrays.stream(transportFilterBeanNames)
				.map(RuntimeBeanReference::new)
				.forEach(transportFilterBeanReferences::add));

		return transportFilterBeanReferences;
	}

	private void registerGatewayTransportFilterDependencies(AnnotationAttributes annotationAttributes,
		BeanDefinitionBuilder gatewayReceiverBeanBuilder) {
		String[] transportFilters = (String[]) annotationAttributes.get(TRANSPORT_FILTERS_LITERAL);
		Optional.ofNullable(transportFilters).ifPresent(
			transportFilerBeanNames -> Arrays.stream(transportFilerBeanNames)
				.forEach(gatewayReceiverBeanBuilder::addDependsOn));
	}

	/* (non-Javadoc) */
	private <T> BeanDefinitionBuilder setPropertyValueIfNotDefault(BeanDefinitionBuilder beanDefinitionBuilder,
		String propertyName, T value, T defaultValue) {

		return (value != null) ?
			beanDefinitionBuilder.addPropertyValue(propertyName, value)
			: beanDefinitionBuilder.addPropertyValue(propertyName, defaultValue);
	}
}
