/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Properties;
import java.util.function.Supplier;

import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanReference;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;
import org.springframework.data.gemfire.wan.OrderPolicyType;
import org.springframework.util.StringUtils;

@Configuration
public class GatewaySenderConfiguration extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	static final int DEFAULT_SOCKET_BUFFER_SIZE = GatewaySender.DEFAULT_SOCKET_BUFFER_SIZE;

	static final boolean DEFAULT_MANUAL_START = GatewaySender.DEFAULT_MANUAL_START;
	static final boolean DEFAULT_DISK_SYNCHRONOUS = GatewaySender.DEFAULT_DISK_SYNCHRONOUS;
	static final boolean DEFAULT_BATCH_CONFLATION_ENABLED = GatewaySender.DEFAULT_BATCH_CONFLATION;
	static final boolean DEFAULT_PARALLEL = GatewaySender.DEFAULT_IS_PARALLEL;
	static final boolean DEFAULT_PERSISTENT = GatewaySender.DEFAULT_PERSISTENCE_ENABLED;

	static final int DEFAULT_ALERT_THRESHOLD = GatewaySender.DEFAULT_ALERT_THRESHOLD;
	static final int DEFAULT_BATCH_SIZE = GatewaySender.DEFAULT_BATCH_SIZE;
	static final int DEFAULT_BATCH_TIME_INTERVAL = GatewaySender.DEFAULT_BATCH_TIME_INTERVAL;
	static final int DEFAULT_DISPATCHER_THREADS = GatewaySender.DEFAULT_DISPATCHER_THREADS;
	static final int DEFAULT_MAXIMUM_QUEUE_MEMORY = GatewaySender.DEFAULT_MAXIMUM_QUEUE_MEMORY;
	static final int DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID = GatewaySender.DEFAULT_DISTRIBUTED_SYSTEM_ID;
	static final int DEFAULT_SOCKET_READ_TIMEOUT = 0;

	static final OrderPolicyType DEFAULT_ORDER_POLICY = OrderPolicyType.KEY;

	static final String DEFAULT_DISK_STORE_REFERENCE = "";
	static final String DEFAULT_EVENT_SUBSTITUTION_FILTER = "";
	static final String DEFAULT_NAME = "GatewaySender";

	static final String[] DEFAULT_EVENT_FILTERS = {};
	static final String[] DEFAULT_TRANSPORT_FILTERS = {};
	static final String[] DEFAULT_REGION_NAMES = {};

	static final String REGION_NAMES_LITERAL = "regions";

	private static final String ALERT_THRESHOLD_LITERAL = "alertThreshold";
	private static final String BATCH_CONFLATION_ENABLED_LITERAL = "batchConflationEnabled";
	private static final String BATCH_SIZE_LITERAL = "batchSize";
	private static final String BATCH_TIME_INTERVAL_LITERAL = "batchTimeInterval";
	private static final String DISK_STORE_REFERENCE_LITERAL = "diskStoreReference";
	private static final String DISK_SYNCHRONOUS_LITERAL = "diskSynchronous";
	private static final String DISPATCHER_THREAD_LITERAL = "dispatcherThreads";
	private static final String EVENT_FILTERS_LITERAL = "eventFilters";
	private static final String EVENT_SUBSTITUTION_FILTER_LITERAL = "eventSubstitutionFilter";
	private static final String MANUAL_START_LITERAL = "manualStart";
	private static final String MAXIMUM_QUEUE_MEMORY_LITERAL = "maximumQueueMemory";
	private static final String NAME_LITERAL = "name";
	private static final String ORDER_POLICY_LITERAL = "orderPolicy";
	private static final String PARALLEL_LITERAL = "parallel";
	private static final String PERSISTENT_LITERAL = "persistent";
	private static final String REMOTE_DIST_SYSTEM_ID_LITERAL = "remoteDistributedSystemId";
	private static final String SOCKET_BUFFER_SIZE_LITERAL = "socketBufferSize";
	private static final String SOCKET_READ_TIMEOUT_LITERAL = "socketReadTimeout";
	private static final String TRANSPORT_FILTERS_LITERAL = "transportFilters";

	private static final String ALERT_THRESHOLD_PROPERTY_NAME = "alert-threshold";
	private static final String BATCH_CONFLATION_ENABLED_PROPERTY_NAME = "batch-conflation-enabled";
	private static final String BATCH_SIZE_PROPERTY_NAME = "batch-size";
	private static final String BATCH_TIME_INTERVAL_PROPERTY_NAME = "batch-time-interval";
	private static final String DISK_STORE_REFERENCE_PROPERTY_NAME = "disk-store-reference";
	private static final String DISK_SYNCHRONOUS_PROPERTY_NAME = "disk-synchronous";
	private static final String DISPATCHER_THREAD_PROPERTY_NAME = "dispatcher-threads";
	private static final String EVENT_FILTERS_PROPERTY_NAME = "event-filters";
	private static final String EVENT_SUBSTITUTION_FILTER_PROPERTY_NAME = "event-substitution-filter";
	private static final String MANUAL_START_PROPERTY_NAME = "manual-start";
	private static final String MAXIMUM_QUEUE_MEMORY_PROPERTY_NAME = "maximum-queue-memory";
	private static final String ORDER_POLICY_PROPERTY_NAME = "order-policy";
	private static final String PARALLEL_PROPERTY_NAME = "parallel";
	private static final String PERSISTENT_PROPERTY_NAME = "persistent";
	private static final String REGION_NAMES_PROPERTY_NAME = "regions";
	private static final String REMOTE_DIST_SYSTEM_ID_PROPERTY_NAME = "remote-distributed-system-id";
	private static final String SOCKET_BUFFER_SIZE_PROPERTY_NAME = "socket-buffer-size";
	private static final String SOCKET_READ_TIMEOUT_PROPERTY_NAME = "socket-read-timeout";
	private static final String TRANSPORT_FILTERS_PROPERTY_NAME = "transport-filters";

	@Autowired(required = false)
	private List<GatewaySenderConfigurer> gatewaySenderConfigurers = Collections.emptyList();

	private String gatewaySenderBeanName;

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableGatewaySender.class;
	}

	protected void setGatewaySenderBeanName(String gatewaySenderBeanName) {
		this.gatewaySenderBeanName = gatewaySenderBeanName;
	}

	/**
	 * Processes the {@link EnableGatewaySender} annotation by configuring and registerig a {@link BeanDefinition}
	 * for a {@link GatewaySender}.
	 *
	 * @see #registerGatewaySender(AnnotationAttributes, AnnotationAttributes, BeanDefinitionRegistry)
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata,
			BeanDefinitionRegistry beanDefinitionRegistry) {

		if (isAnnotationPresent(annotationMetadata)) {

			AnnotationAttributes gatewaySenderAnnotation = getAnnotationAttributes(annotationMetadata);

			registerGatewaySender(gatewaySenderAnnotation, null, beanDefinitionRegistry);
		}
	}

	/**
	 * Processes a defined {@link EnableGatewaySender} on the {@link EnableGatewaySenders} annotation.
	 * Processes properties defined in either the {@link Annotation} or {@literal application.properties}.
	 *
	 * @see #registerGatewaySender(String, AnnotationAttributes, AnnotationAttributes, BeanDefinitionRegistry)
	 */
	protected void registerGatewaySender(AnnotationAttributes gatewaySenderAnnotation,
			AnnotationAttributes parentGatewaySenderAnnotation, BeanDefinitionRegistry registry) {

		String gatewaySenderName = getStringFromAnnotation(gatewaySenderAnnotation, NAME_LITERAL);

		registerGatewaySender(gatewaySenderName, gatewaySenderAnnotation, parentGatewaySenderAnnotation, registry);
	}

	/**
	 * Processes a defined {@link EnableGatewaySender} on the {@link EnableGatewaySenders} annotation.
	 * Processes properties defined in either the {@link Annotation} or {@literal application.properties{@literal}.
	 */
	protected void registerGatewaySender(String gatewaySenderName, AnnotationAttributes gatewaySenderAnnotation,
			AnnotationAttributes parentGatewaySenderAnnotation, BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder gatewaySenderBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(GatewaySenderFactoryBean.class);

		configureGatewaySenderFromAnnotation(gatewaySenderName, gatewaySenderAnnotation,
			parentGatewaySenderAnnotation, gatewaySenderBuilder);

		configureGatewaySenderFromProperties(gatewaySenderName, gatewaySenderBuilder);

		configureGatewaySenderArguments(gatewaySenderName, gatewaySenderAnnotation,
			parentGatewaySenderAnnotation, gatewaySenderBuilder);

		registry.registerBeanDefinition(gatewaySenderName, gatewaySenderBuilder.getBeanDefinition());
	}

	private void configureGatewaySenderArguments(String gatewaySenderName,
			AnnotationAttributes gatewaySenderAnnotation, AnnotationAttributes parentGatewaySenderAnnotation,
			BeanDefinitionBuilder gatewaySenderBuilder) {

		String[] annotationRegionNames =
			Optional.ofNullable(getStringArrayFromAnnotation(gatewaySenderAnnotation, REGION_NAMES_LITERAL))
			.filter(ArrayUtils::isNotEmpty)
			.orElseGet(() ->
				Optional.ofNullable(getStringArrayFromAnnotation(parentGatewaySenderAnnotation, REGION_NAMES_LITERAL))
					.filter(ArrayUtils::isNotEmpty)
					.orElse(DEFAULT_REGION_NAMES));

		String[] resolvedRegionNames = Optional
			.ofNullable(resolveValueFromProperty(gatewaySenderName, REGION_NAMES_PROPERTY_NAME, annotationRegionNames))
			.filter(ArrayUtils::isNotEmpty)
			.orElse(annotationRegionNames);

		setPropertyValueOrUseParent(gatewaySenderBuilder, REGION_NAMES_LITERAL, resolvedRegionNames, null,
			DEFAULT_REGION_NAMES);
	}

	/**
	 * Configures a {@link GatewaySender} from attributes set with the {@link EnableGatewaySender} annotation.
	 */
	private void configureGatewaySenderFromAnnotation(String gatewaySenderName,
			AnnotationAttributes gatewaySenderAnnotation, AnnotationAttributes parentGatewaySenderAnnotation,
			BeanDefinitionBuilder gatewaySenderBuilder) {

		setGatewaySenderBeanName(Optional.ofNullable(gatewaySenderName).orElse(DEFAULT_NAME));

		setPropertyValueIfNotDefault(gatewaySenderBuilder, NAME_LITERAL, gatewaySenderName, DEFAULT_NAME);

		setPropertyValueOrUseParent(gatewaySenderBuilder, ALERT_THRESHOLD_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, ALERT_THRESHOLD_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, ALERT_THRESHOLD_LITERAL),
			DEFAULT_ALERT_THRESHOLD);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_CONFLATION_ENABLED_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, BATCH_CONFLATION_ENABLED_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, BATCH_CONFLATION_ENABLED_LITERAL),
			DEFAULT_BATCH_CONFLATION_ENABLED);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_SIZE_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, BATCH_SIZE_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, BATCH_SIZE_LITERAL),
			DEFAULT_BATCH_SIZE);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_TIME_INTERVAL_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, BATCH_TIME_INTERVAL_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, BATCH_TIME_INTERVAL_LITERAL),
			DEFAULT_BATCH_TIME_INTERVAL);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISK_STORE_REFERENCE_LITERAL,
			getStringFromAnnotation(gatewaySenderAnnotation, DISK_STORE_REFERENCE_LITERAL),
			getStringFromAnnotation(parentGatewaySenderAnnotation, DISK_STORE_REFERENCE_LITERAL),
			DEFAULT_DISK_STORE_REFERENCE);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISK_SYNCHRONOUS_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, DISK_SYNCHRONOUS_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, DISK_SYNCHRONOUS_LITERAL),
			DEFAULT_DISK_SYNCHRONOUS);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISPATCHER_THREAD_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, DISPATCHER_THREAD_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, DISPATCHER_THREAD_LITERAL),
			DEFAULT_DISPATCHER_THREADS);

		setPropertyValueOrUseParentAsBeanReferenceList(gatewaySenderBuilder, EVENT_FILTERS_LITERAL,
			getStringArrayFromAnnotation(gatewaySenderAnnotation, EVENT_FILTERS_LITERAL),
			getStringArrayFromAnnotation(parentGatewaySenderAnnotation, EVENT_FILTERS_LITERAL),
			DEFAULT_EVENT_FILTERS);

		setPropertyValueIfNotDefaultAsBeanReference(gatewaySenderBuilder, EVENT_SUBSTITUTION_FILTER_LITERAL,
			getStringFromAnnotation(gatewaySenderAnnotation, EVENT_SUBSTITUTION_FILTER_LITERAL),
			getStringFromAnnotation(parentGatewaySenderAnnotation, EVENT_SUBSTITUTION_FILTER_LITERAL),
			DEFAULT_EVENT_SUBSTITUTION_FILTER);

		setPropertyValueOrUseParent(gatewaySenderBuilder, MANUAL_START_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, MANUAL_START_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, MANUAL_START_LITERAL),
			DEFAULT_MANUAL_START);

		setPropertyValueOrUseParent(gatewaySenderBuilder, MAXIMUM_QUEUE_MEMORY_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, MAXIMUM_QUEUE_MEMORY_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, MAXIMUM_QUEUE_MEMORY_LITERAL),
			DEFAULT_MAXIMUM_QUEUE_MEMORY);

		setPropertyValueOrUseParent(gatewaySenderBuilder, ORDER_POLICY_LITERAL,
			getEnumFromAnnotation(gatewaySenderAnnotation, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY).toString(),
			getEnumFromAnnotation(parentGatewaySenderAnnotation, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY).toString(),
			DEFAULT_ORDER_POLICY.toString());

		setPropertyValueOrUseParent(gatewaySenderBuilder, PARALLEL_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, PARALLEL_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, PARALLEL_LITERAL),
			DEFAULT_PARALLEL);

		setPropertyValueOrUseParent(gatewaySenderBuilder, PERSISTENT_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, PERSISTENT_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, PERSISTENT_LITERAL),
			DEFAULT_PERSISTENT);

		setPropertyValueOrUseParent(gatewaySenderBuilder, REMOTE_DIST_SYSTEM_ID_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, REMOTE_DIST_SYSTEM_ID_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, REMOTE_DIST_SYSTEM_ID_LITERAL),
			DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID);

		setPropertyValueOrUseParent(gatewaySenderBuilder, SOCKET_BUFFER_SIZE_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, SOCKET_BUFFER_SIZE_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, SOCKET_BUFFER_SIZE_LITERAL),
			DEFAULT_SOCKET_BUFFER_SIZE);

		setPropertyValueOrUseParent(gatewaySenderBuilder, SOCKET_READ_TIMEOUT_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, SOCKET_READ_TIMEOUT_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, SOCKET_READ_TIMEOUT_LITERAL),
			DEFAULT_SOCKET_READ_TIMEOUT);

		setPropertyValueOrUseParentAsBeanReferenceList(gatewaySenderBuilder, TRANSPORT_FILTERS_LITERAL,
			getStringArrayFromAnnotation(gatewaySenderAnnotation, TRANSPORT_FILTERS_LITERAL),
			getStringArrayFromAnnotation(parentGatewaySenderAnnotation, TRANSPORT_FILTERS_LITERAL),
			DEFAULT_TRANSPORT_FILTERS);
	}

	/**
	 * Configures a {@link GatewaySender} from {@link Properties} defined in {@literal application.properties}.
	 *
	 * These properties are <i>"named"</i> properties and will follow the following pattern:
	 * <b><i>{@literal spring.data.gemfire.gateway.sender.<name>.manual-start}</i></b>
	 */
	private void configureGatewaySenderFromProperties(String gatewaySenderName,
			BeanDefinitionBuilder gatewaySenderBuilder) {

		gatewaySenderBuilder.addPropertyValue("gatewaySenderConfigurers", resolveGatewaySenderConfigurers());

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			ALERT_THRESHOLD_PROPERTY_NAME, ALERT_THRESHOLD_LITERAL, DEFAULT_ALERT_THRESHOLD);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			BATCH_CONFLATION_ENABLED_PROPERTY_NAME, BATCH_CONFLATION_ENABLED_LITERAL, DEFAULT_BATCH_CONFLATION_ENABLED);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			BATCH_SIZE_PROPERTY_NAME, BATCH_SIZE_LITERAL, DEFAULT_BATCH_SIZE);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			BATCH_TIME_INTERVAL_PROPERTY_NAME, BATCH_TIME_INTERVAL_LITERAL, DEFAULT_BATCH_TIME_INTERVAL);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			DISK_STORE_REFERENCE_PROPERTY_NAME, DISK_STORE_REFERENCE_LITERAL, new String[0]);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			DISK_SYNCHRONOUS_PROPERTY_NAME, DISK_SYNCHRONOUS_LITERAL, DEFAULT_DISK_SYNCHRONOUS);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			DISPATCHER_THREAD_PROPERTY_NAME, DISPATCHER_THREAD_LITERAL, DEFAULT_DISPATCHER_THREADS);

		configureBeanReferenceListFromProperty(gatewaySenderBuilder, gatewaySenderName,
			EVENT_FILTERS_PROPERTY_NAME, EVENT_FILTERS_LITERAL, DEFAULT_EVENT_FILTERS);

		configureBeanReferenceFromProperty(gatewaySenderBuilder, gatewaySenderName,
			EVENT_SUBSTITUTION_FILTER_PROPERTY_NAME, EVENT_SUBSTITUTION_FILTER_LITERAL,
			DEFAULT_EVENT_SUBSTITUTION_FILTER);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			MANUAL_START_PROPERTY_NAME, MANUAL_START_LITERAL, DEFAULT_MANUAL_START);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			MAXIMUM_QUEUE_MEMORY_PROPERTY_NAME, MAXIMUM_QUEUE_MEMORY_LITERAL, DEFAULT_MAXIMUM_QUEUE_MEMORY);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			ORDER_POLICY_PROPERTY_NAME, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY.toString());

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			PARALLEL_PROPERTY_NAME, PARALLEL_LITERAL, DEFAULT_PARALLEL);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			PERSISTENT_PROPERTY_NAME, PERSISTENT_LITERAL, DEFAULT_PERSISTENT);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			REMOTE_DIST_SYSTEM_ID_PROPERTY_NAME, REMOTE_DIST_SYSTEM_ID_LITERAL, DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			SOCKET_BUFFER_SIZE_PROPERTY_NAME, SOCKET_BUFFER_SIZE_LITERAL, DEFAULT_SOCKET_BUFFER_SIZE);

		configureGatewaySenderFromProperty(gatewaySenderBuilder, gatewaySenderName,
			SOCKET_READ_TIMEOUT_PROPERTY_NAME, SOCKET_READ_TIMEOUT_LITERAL, DEFAULT_SOCKET_READ_TIMEOUT);

		configureBeanReferenceListFromProperty(gatewaySenderBuilder, gatewaySenderName,
			TRANSPORT_FILTERS_PROPERTY_NAME, TRANSPORT_FILTERS_LITERAL, DEFAULT_TRANSPORT_FILTERS);
	}

	/**
	 * Configures a {@link GatewaySender} from {@link Properties} defined in {@literal application.properties}.
	 *
	 * These properties are {@literal "named"} properties and will follow the following pattern:
	 * {@literal spring.data.gemfire.gateway.sender.<name>.manual-start}.
	 */
	private <T> void configureGatewaySenderFromProperty(BeanDefinitionBuilder gatewaySenderBuilder,
			String gatewaySenderName, String propertyName, String fieldName, T defaultValue) {

		T resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);

		if (resolvedPropertyValue != null) {
			setPropertyValueOrUseParent(gatewaySenderBuilder, fieldName, resolvedPropertyValue, null,
				defaultValue);
		}
	}

	private Boolean getBooleanFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getBoolean(annotationLabel));
	}

	private Enum<?> getEnumFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel,
			Enum<?> defaultValue) {

		return getValueFromAnnotation(gatewaySenderAnnotation, () -> gatewaySenderAnnotation.getEnum(annotationLabel),
			defaultValue);
	}

	private Integer getNumberFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getNumber(annotationLabel));
	}

	private String getStringFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getString(annotationLabel));
	}

	private String[] getStringArrayFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getStringArray(annotationLabel));
	}

	private <T> T getValueFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, Supplier<T> supplier) {
		return gatewaySenderAnnotation != null ? supplier.get() : null;
	}

	private <T> T getValueFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, Supplier<T> supplier,
			T defaultValue) {

		return gatewaySenderAnnotation != null ? supplier.get() : defaultValue;
	}

	private void configureBeanReferenceFromProperty(BeanDefinitionBuilder gatewaySenderBeanBuilder,
			String gatewaySenderName, String propertyName, String fieldName, String defaultValue) {

		String resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);

		if (resolvedPropertyValue != null) {
			setPropertyValueIfNotDefaultAsBeanReference(gatewaySenderBeanBuilder, fieldName, resolvedPropertyValue,
				null, defaultValue);
		}
	}

	private void configureBeanReferenceListFromProperty(BeanDefinitionBuilder gatewaySenderBeanBuilder,
			String gatewaySenderName, String propertyName, String fieldName, String[] defaultValue) {

		String[] resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);

		if (resolvedPropertyValue != null) {
			setPropertyValueOrUseParentAsBeanReferenceList(gatewaySenderBeanBuilder, fieldName, resolvedPropertyValue,
				new String[0], defaultValue);
		}
	}

	private List<GatewaySenderConfigurer> resolveGatewaySenderConfigurers() {

		return Optional.ofNullable(this.gatewaySenderConfigurers)
			.filter(gatewaySenderConfigurers -> !gatewaySenderConfigurers.isEmpty())
			.orElseGet(() ->
				Collections.singletonList(LazyResolvingComposableGatewaySenderConfigurer.create(getBeanFactory())));
	}

	@SuppressWarnings("unchecked")
	private <T> T resolveValueFromProperty(String gatewaySenderName, String propertyName, T defaultValue) {

		Class<T> type = (Class<T>) defaultValue.getClass();

		T gatewaySenderProperty = resolveProperty(gatewaySenderProperty(propertyName), type, null);

		T namedGatewaySenderProperty =
			resolveProperty(namedGatewaySenderProperty(gatewaySenderName, propertyName), type, null);

		return namedGatewaySenderProperty != null ? namedGatewaySenderProperty : gatewaySenderProperty;
	}


	private <T> BeanDefinitionBuilder setPropertyValueIfNotDefault(BeanDefinitionBuilder beanDefinitionBuilder,
			String propertyName, T value, T defaultValue) {

		return beanDefinitionBuilder.addPropertyValue(propertyName, Optional.ofNullable(value).orElse(defaultValue));
	}

	private <T> BeanDefinitionBuilder setPropertyValueOrUseParent(BeanDefinitionBuilder beanDefinitionBuilder,
			String propertyName, T value, T parentValue, T defaultValue) {

		T resolvedValue = Optional.ofNullable(value)
			.filter(childValue -> !childValue.equals(defaultValue))
			.orElseGet(() -> Optional.ofNullable(parentValue).orElse(defaultValue));

		return beanDefinitionBuilder.addPropertyValue(propertyName, resolvedValue);
	}

	private BeanDefinitionBuilder setPropertyValueIfNotDefaultAsBeanReference(
			BeanDefinitionBuilder beanDefinitionBuilder, String propertyName, String value, String parentValue,
			String defaultValue) {

		if (!StringUtils.isEmpty(value)) {
			return beanDefinitionBuilder.addPropertyReference(propertyName, value);
		}
		else if (!StringUtils.isEmpty(parentValue)) {
			return beanDefinitionBuilder.addPropertyReference(propertyName, parentValue);
		}
		else {
			if (!StringUtils.isEmpty(defaultValue)) {
				beanDefinitionBuilder.addPropertyReference(propertyName, defaultValue);
			}
		}

		return beanDefinitionBuilder;
	}

	private BeanDefinitionBuilder setPropertyValueOrUseParentAsBeanReferenceList(
			BeanDefinitionBuilder beanDefinitionBuilder, String propertyName, String[] values, String[] parentValues,
			String[] defaultList) {

		String[] resolvedList = Optional.ofNullable(values)
			.filter(it -> !Arrays.equals(it, defaultList))
			.orElseGet(() -> Optional.ofNullable(parentValues).orElse(defaultList));

		ManagedList<BeanReference> beanReferences = new ManagedList<>();

		Arrays.stream(resolvedList)
			.map(RuntimeBeanReference::new)
			.forEach(beanReferences::add);

		return beanDefinitionBuilder.addPropertyValue(propertyName, beanReferences);
	}
}
