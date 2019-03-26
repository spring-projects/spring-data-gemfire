package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

import org.apache.geode.cache.wan.GatewaySender;
import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.factory.config.BeanReference;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;
import org.springframework.data.gemfire.wan.OrderPolicyType;
import org.springframework.util.StringUtils;

@Configuration
public class GatewaySenderConfiguration extends AbstractAnnotationConfigSupport
	implements ImportBeanDefinitionRegistrar, ImportAware {

	public static final String DEFAULT_NAME = "GatewaySender";
	public static final int DEFAULT_SOCKET_BUFFER_SIZE = GatewaySender.DEFAULT_SOCKET_BUFFER_SIZE;
	static final boolean DEFAULT_MANUAL_START = GatewaySender.DEFAULT_MANUAL_START;
	static final int DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID = GatewaySender.DEFAULT_DISTRIBUTED_SYSTEM_ID;
	static final boolean DEFAULT_DISK_SYNCHRONOUS = GatewaySender.DEFAULT_DISK_SYNCHRONOUS;
	static final boolean DEFAULT_BATCH_CONFLATION_ENABLED = GatewaySender.DEFAULT_BATCH_CONFLATION;
	static final boolean DEFAULT_PARALLEL = GatewaySender.DEFAULT_IS_PARALLEL;
	static final boolean DEFAULT_PERSISTENT = GatewaySender.DEFAULT_PERSISTENCE_ENABLED;
	static final int DEFAULT_ALERT_THRESHOLD = GatewaySender.DEFAULT_ALERT_THRESHOLD;
	static final int DEFAULT_BATCH_SIZE = GatewaySender.DEFAULT_BATCH_SIZE;
	static final int DEFAULT_BATCH_TIME_INTERVAL = GatewaySender.DEFAULT_BATCH_TIME_INTERVAL;
	static final int DEFAULT_DISPATCHER_THREADS = GatewaySender.DEFAULT_DISPATCHER_THREADS;
	static final int DEFAULT_MAXIMUM_QUEUE_MEMORY = GatewaySender.DEFAULT_MAXIMUM_QUEUE_MEMORY;
	static final int DEFAULT_SOCKET_READ_TIMEOUT = 0;
	static final OrderPolicyType DEFAULT_ORDER_POLICY = OrderPolicyType.KEY;
	static final String DEFAULT_EVENT_SUBSTITUTION_FILTER = "";
	static final String[] DEFAULT_EVENT_FILTERS = {};
	static final String[] DEFAULT_TRANSPORT_FILTERS = {};
	static final String[] DEFAULT_REGION_NAMES = {};
	static final String DEFAULT_DISK_STORE_REFERENCE = "";

	static final String REGION_NAMES_LITERAL = "regions";
	private static final String NAME_LITERAL = "name";
	private static final String MANUAL_START_LITERAL = "manualStart";
	private static final String REMOTE_DIST_SYSTEM_ID_LITERAL = "remoteDistributedSystemId";
	private static final String DISK_SYNCHRONOUS_LITERAL = "diskSynchronous";
	private static final String BATCH_CONFLATION_ENABLED_LITERAL = "batchConflationEnabled";
	private static final String PARALLEL_LITERAL = "parallel";
	private static final String PERSISTENT_LITERAL = "persistent";
	private static final String ORDER_POLICY_LITERAL = "orderPolicy";
	private static final String EVENT_SUBSTITUTION_FILTER_LITERAL = "eventSubstitutionFilter";
	private static final String ALERT_THRESHOLD_LITERAL = "alertThreshold";
	private static final String BATCH_SIZE_LITERAL = "batchSize";
	private static final String BATCH_TIME_INTERVAL_LITERAL = "batchTimeInterval";
	private static final String DISPATCHER_THREAD_LITERAL = "dispatcherThreads";
	private static final String MAXIMUM_QUEUE_MEMORY_LITERAL = "maximumQueueMemory";
	private static final String SOCKET_BUFFER_SIZE_LITERAL = "socketBufferSize";
	private static final String SOCKET_READ_TIMEOUT_LITERAL = "socketReadTimeout";
	private static final String DISK_STORE_REFERENCE_LITERAL = "diskStoreReference";
	private static final String EVENT_FILTERS_LITERAL = "eventFilters";
	private static final String TRANSPORT_FILTERS_LITERAL = "transportFilters";

	private static final String MANUAL_START_PROPERTY_NAME = "manual-start";
	private static final String REMOTE_DIST_SYSTEM_ID_PROPERTY_NAME = "remote-distributed-system-id";
	private static final String DISK_SYNCHRONOUS_PROPERTY_NAME = "disk-synchronous";
	private static final String BATCH_CONFLATION_ENABLED_PROPERTY_NAME = "batch-conflation-enabled";
	private static final String PARALLEL_PROPERTY_NAME = "parallel";
	private static final String PERSISTENT_PROPERTY_NAME = "persistent";
	private static final String ORDER_POLICY_PROPERTY_NAME = "order-policy";
	private static final String EVENT_SUBSTITUTION_FILTER_PROPERTY_NAME = "event-substitution-filter";
	private static final String ALERT_THRESHOLD_PROPERTY_NAME = "alert-threshold";
	private static final String BATCH_SIZE_PROPERTY_NAME = "batch-size";
	private static final String BATCH_TIME_INTERVAL_PROPERTY_NAME = "batch-time-interval";
	private static final String DISPATCHER_THREAD_PROPERTY_NAME = "dispatcher-threads";
	private static final String MAXIMUM_QUEUE_MEMORY_PROPERTY_NAME = "maximum-queue-memory";
	private static final String SOCKET_BUFFER_SIZE_PROPERTY_NAME = "socket-buffer-size";
	private static final String SOCKET_READ_TIMEOUT_PROPERTY_NAME = "socket-read-timeout";
	private static final String DISK_STORE_REFERENCE_PROPERTY_NAME = "disk-store-reference";
	private static final String EVENT_FILTERS_PROPERTY_NAME = "event-filters";
	private static final String TRANSPORT_FILTERS_PROPERTY_NAME = "transport-filters";
	private static final String REGION_NAMES_PROPERTY_NAME = "regions";

	private String gatewaySenderBeanName;
	private List<GatewaySenderConfigurer> gatewaySenderConfigurers = Collections.emptyList();

	/**
	 * Processes the {@link EnableGatewaySender} annotation and registers the configured BeanDefinition
	 *
	 * @param annotationMetadata
	 * @param beanDefinitionRegistry
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata,
		BeanDefinitionRegistry beanDefinitionRegistry) {

		if (annotationMetadata.hasAnnotation(EnableGatewaySender.class.getName())) {
			Map<String, Object> annotationAttributesMap = annotationMetadata
				.getAnnotationAttributes(EnableGatewaySender.class.getName());

			AnnotationAttributes gatewaySenderAnnotation = AnnotationAttributes.fromMap(annotationAttributesMap);

			registerGatewaySender(gatewaySenderAnnotation, beanDefinitionRegistry, null);
		}
	}

	@Override protected Class<? extends Annotation> getAnnotationType() {
		return EnableGatewaySender.class;
	}


	/**
	 * This method processes a defined {@link EnableGatewaySender} on the {@link EnableGatewaySenders} annotation.
	 * It will process properties defined in either annotation or <b><i>application.properties</i></b>.
	 *
	 * @param gatewaySenderAnnotation
	 * @param registry
	 * @param parentGatewaySenderAnnotation
	 */
	protected void registerGatewaySender(String gatewaySenderName, AnnotationAttributes gatewaySenderAnnotation,
		BeanDefinitionRegistry registry, AnnotationAttributes parentGatewaySenderAnnotation) {

		BeanDefinitionBuilder gatewaySenderBuilder = BeanDefinitionBuilder.genericBeanDefinition(
			GatewaySenderFactoryBean.class);

		configureGatewaySenderFromAnnotation(gatewaySenderName, gatewaySenderAnnotation, gatewaySenderBuilder,
			parentGatewaySenderAnnotation);

		configureGatewaySenderFromProperties(gatewaySenderName, gatewaySenderBuilder);

		configureGatewaySenderArguments(gatewaySenderName, gatewaySenderAnnotation, gatewaySenderBuilder,
			parentGatewaySenderAnnotation);

		registry.registerBeanDefinition(gatewaySenderName, gatewaySenderBuilder.getBeanDefinition());
	}

	private void configureGatewaySenderArguments(String gatewaySenderName,
		AnnotationAttributes gatewaySenderAnnotation, BeanDefinitionBuilder gatewaySenderBuilder,
		AnnotationAttributes parentGatewaySenderAnnotation) {

		String[] resolveValue = Optional
			.ofNullable(getStringArrayFromAnnotation(gatewaySenderAnnotation, REGION_NAMES_LITERAL))
			.filter(childValue -> !childValue.equals(DEFAULT_REGION_NAMES))
			.orElse(
				Optional.ofNullable(getStringArrayFromAnnotation(parentGatewaySenderAnnotation, REGION_NAMES_LITERAL))
					.orElse(DEFAULT_REGION_NAMES));

		String[] resolvedPropertyValue = Optional
			.ofNullable(resolveValueFromProperty(gatewaySenderName, REGION_NAMES_PROPERTY_NAME, resolveValue))
			.orElse(resolveValue);

		setPropertyValueOrUseParent(gatewaySenderBuilder, REGION_NAMES_LITERAL, resolvedPropertyValue, null,
			DEFAULT_REGION_NAMES);
	}

	/**
	 * This method processes a defined {@link EnableGatewaySender} on the {@link EnableGatewaySenders} annotation.
	 * It will process properties defined in either annotation or <b><i>application.properties</i></b>.
	 *
	 * @param gatewaySenderAnnotation
	 * @param registry
	 * @param parentGatewaySenderAnnotation
	 */
	protected void registerGatewaySender(AnnotationAttributes gatewaySenderAnnotation,
		BeanDefinitionRegistry registry, AnnotationAttributes parentGatewaySenderAnnotation) {

		String gatewaySenderName = getStringFromAnnotation(gatewaySenderAnnotation, NAME_LITERAL);

		registerGatewaySender(gatewaySenderName, gatewaySenderAnnotation, registry, parentGatewaySenderAnnotation);
	}

	public void setGatewaySenderBeanName(String gatewaySenderBeanName) {
		this.gatewaySenderBeanName = gatewaySenderBeanName;
	}

	/**
	 * In this method a {@link GatewaySender} is configured from properties defined on the {@link EnableGatewaySender}
	 * annotation
	 *
	 * @param gatewaySenderAnnotation
	 * @param gatewaySenderBuilder
	 * @param parentGatewaySenderAnnotation
	 */
	private void configureGatewaySenderFromAnnotation(String gatewaySenderName,
		AnnotationAttributes gatewaySenderAnnotation,
		BeanDefinitionBuilder gatewaySenderBuilder,
		AnnotationAttributes parentGatewaySenderAnnotation) {

		setGatewaySenderBeanName(Optional.ofNullable(gatewaySenderName).orElse(DEFAULT_NAME));
		setPropertyValueIfNotDefault(gatewaySenderBuilder, NAME_LITERAL,
			gatewaySenderName, DEFAULT_NAME);

		setPropertyValueOrUseParent(gatewaySenderBuilder, MANUAL_START_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, MANUAL_START_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, MANUAL_START_LITERAL), DEFAULT_MANUAL_START);

		setPropertyValueOrUseParent(gatewaySenderBuilder, REMOTE_DIST_SYSTEM_ID_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, REMOTE_DIST_SYSTEM_ID_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, REMOTE_DIST_SYSTEM_ID_LITERAL),
			DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISK_STORE_REFERENCE_LITERAL,
			getStringFromAnnotation(gatewaySenderAnnotation, DISK_STORE_REFERENCE_LITERAL),
			getStringFromAnnotation(parentGatewaySenderAnnotation, DISK_STORE_REFERENCE_LITERAL),
			DEFAULT_DISK_STORE_REFERENCE);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISK_SYNCHRONOUS_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, DISK_SYNCHRONOUS_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, DISK_SYNCHRONOUS_LITERAL),
			DEFAULT_DISK_SYNCHRONOUS);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_CONFLATION_ENABLED_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, BATCH_CONFLATION_ENABLED_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, BATCH_CONFLATION_ENABLED_LITERAL),
			DEFAULT_BATCH_CONFLATION_ENABLED);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_SIZE_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, BATCH_SIZE_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, BATCH_SIZE_LITERAL), DEFAULT_BATCH_SIZE);

		setPropertyValueOrUseParent(gatewaySenderBuilder, BATCH_TIME_INTERVAL_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, BATCH_TIME_INTERVAL_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, BATCH_TIME_INTERVAL_LITERAL),
			DEFAULT_BATCH_TIME_INTERVAL);

		setPropertyValueOrUseParent(gatewaySenderBuilder, PARALLEL_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, PARALLEL_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, PARALLEL_LITERAL), DEFAULT_PARALLEL);

		setPropertyValueOrUseParent(gatewaySenderBuilder, PERSISTENT_LITERAL,
			getBooleanFromAnnotation(gatewaySenderAnnotation, PERSISTENT_LITERAL),
			getBooleanFromAnnotation(parentGatewaySenderAnnotation, PERSISTENT_LITERAL), DEFAULT_PERSISTENT);

		setPropertyValueOrUseParent(gatewaySenderBuilder, ORDER_POLICY_LITERAL,
			getEnumFromAnnotation(gatewaySenderAnnotation, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY).toString(),
			getEnumFromAnnotation(parentGatewaySenderAnnotation, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY).toString(),
			DEFAULT_ORDER_POLICY.toString());

		setPropertyValueOrUseParentAsBeanReferenceList(gatewaySenderBuilder, EVENT_FILTERS_LITERAL,
			getStringArrayFromAnnotation(gatewaySenderAnnotation, EVENT_FILTERS_LITERAL),
			getStringArrayFromAnnotation(parentGatewaySenderAnnotation, EVENT_FILTERS_LITERAL), DEFAULT_EVENT_FILTERS);

		setPropertyValueIfNotDefaultAsBeanReference(gatewaySenderBuilder, EVENT_SUBSTITUTION_FILTER_LITERAL,
			getStringFromAnnotation(gatewaySenderAnnotation, EVENT_SUBSTITUTION_FILTER_LITERAL),
			getStringFromAnnotation(parentGatewaySenderAnnotation, EVENT_SUBSTITUTION_FILTER_LITERAL),
			DEFAULT_EVENT_SUBSTITUTION_FILTER);

		setPropertyValueOrUseParent(gatewaySenderBuilder, ALERT_THRESHOLD_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, ALERT_THRESHOLD_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, ALERT_THRESHOLD_LITERAL), DEFAULT_ALERT_THRESHOLD);

		setPropertyValueOrUseParent(gatewaySenderBuilder, DISPATCHER_THREAD_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, DISPATCHER_THREAD_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, DISPATCHER_THREAD_LITERAL),
			DEFAULT_DISPATCHER_THREADS);

		setPropertyValueOrUseParent(gatewaySenderBuilder, MAXIMUM_QUEUE_MEMORY_LITERAL,
			getNumberFromAnnotation(gatewaySenderAnnotation, MAXIMUM_QUEUE_MEMORY_LITERAL),
			getNumberFromAnnotation(parentGatewaySenderAnnotation, MAXIMUM_QUEUE_MEMORY_LITERAL),
			DEFAULT_MAXIMUM_QUEUE_MEMORY);

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

	private Enum<?> getEnumFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel,
		Enum<?> defaultValue) {
		Enum returnValue = defaultValue;
		if (gatewaySenderAnnotation != null) {
			returnValue = getValueFromAnnotation(gatewaySenderAnnotation, () ->
				gatewaySenderAnnotation.getEnum(annotationLabel));
		}
		return returnValue;
	}

	private String getStringFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getString(annotationLabel));
	}

	private Integer getNumberFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getNumber(annotationLabel));
	}

	private String[] getStringArrayFromAnnotation(AnnotationAttributes gatewaySenderAnnotation,
		String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getStringArray(annotationLabel));
	}

	private Boolean getBooleanFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, String annotationLabel) {
		return getValueFromAnnotation(gatewaySenderAnnotation,
			() -> gatewaySenderAnnotation.getBoolean(annotationLabel));
	}

	private <T> T getValueFromAnnotation(AnnotationAttributes gatewaySenderAnnotation, Supplier<T> supplier) {
		if (gatewaySenderAnnotation == null) {
			return null;
		}
		else {
			return supplier.get();
		}
	}

	/**
	 * In this method a {@link GatewaySender} is configured from properties defined within an <b><i>application.properties</i></b>
	 * file.
	 * These properties are <i>"named"</i> properties and will follow the following pattern:
	 * <b><i>{@literal spring.data.gemfire.gateway.sender.<name>.manual-start}</i></b>
	 *
	 * @param gatewaySenderBeanBuilder
	 */
	private void configureGatewaySenderFromProperties(String gatewaySenderName,
		BeanDefinitionBuilder gatewaySenderBeanBuilder) {
		MutablePropertyValues beanPropertyValues = gatewaySenderBeanBuilder.getRawBeanDefinition().getPropertyValues();

		gatewaySenderBeanBuilder.addPropertyValue("gatewaySenderConfigurers", resolveGatewaySenderConfigurers());

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			MANUAL_START_PROPERTY_NAME, MANUAL_START_LITERAL, DEFAULT_MANUAL_START);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			REMOTE_DIST_SYSTEM_ID_PROPERTY_NAME, REMOTE_DIST_SYSTEM_ID_LITERAL, DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			DISK_STORE_REFERENCE_PROPERTY_NAME, DISK_STORE_REFERENCE_LITERAL, new String[0]);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			DISK_SYNCHRONOUS_PROPERTY_NAME, DISK_SYNCHRONOUS_LITERAL, DEFAULT_DISK_SYNCHRONOUS);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			BATCH_CONFLATION_ENABLED_PROPERTY_NAME, BATCH_CONFLATION_ENABLED_LITERAL, DEFAULT_BATCH_CONFLATION_ENABLED);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			BATCH_SIZE_PROPERTY_NAME, BATCH_SIZE_LITERAL, DEFAULT_BATCH_SIZE);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			BATCH_TIME_INTERVAL_PROPERTY_NAME, BATCH_TIME_INTERVAL_LITERAL, DEFAULT_BATCH_TIME_INTERVAL);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			PARALLEL_PROPERTY_NAME, PARALLEL_LITERAL, DEFAULT_PARALLEL);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			PERSISTENT_PROPERTY_NAME, PERSISTENT_LITERAL, DEFAULT_PERSISTENT);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			ORDER_POLICY_PROPERTY_NAME, ORDER_POLICY_LITERAL, DEFAULT_ORDER_POLICY.toString());

		configureBeanReferenceListFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			EVENT_FILTERS_PROPERTY_NAME, EVENT_FILTERS_LITERAL, DEFAULT_EVENT_FILTERS);

		configureBeanReferenceFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			EVENT_SUBSTITUTION_FILTER_PROPERTY_NAME, EVENT_SUBSTITUTION_FILTER_LITERAL,
			DEFAULT_EVENT_SUBSTITUTION_FILTER);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			ALERT_THRESHOLD_PROPERTY_NAME, ALERT_THRESHOLD_LITERAL, DEFAULT_ALERT_THRESHOLD);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			DISPATCHER_THREAD_PROPERTY_NAME, DISPATCHER_THREAD_LITERAL, DEFAULT_DISPATCHER_THREADS);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			MAXIMUM_QUEUE_MEMORY_PROPERTY_NAME, MAXIMUM_QUEUE_MEMORY_LITERAL, DEFAULT_MAXIMUM_QUEUE_MEMORY);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			SOCKET_BUFFER_SIZE_PROPERTY_NAME, SOCKET_BUFFER_SIZE_LITERAL, DEFAULT_SOCKET_BUFFER_SIZE);

		configureGatewaySenderFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			SOCKET_READ_TIMEOUT_PROPERTY_NAME, SOCKET_READ_TIMEOUT_LITERAL, DEFAULT_SOCKET_READ_TIMEOUT);

		configureBeanReferenceListFromProperty(gatewaySenderBeanBuilder, gatewaySenderName,
			TRANSPORT_FILTERS_PROPERTY_NAME, TRANSPORT_FILTERS_LITERAL, DEFAULT_TRANSPORT_FILTERS);
	}

	/**
	 * In this method a {@link GatewaySender} is configured from properties defined within an <b><i>application.properties</i></b>
	 * file.
	 * These properties are <i>"named"</i> properties and will follow the following pattern:
	 * <b><i>{@literal spring.data.gemfire.gateway.sender.<name>.manual-start}</i></b>
	 *
	 * @param gatewaySenderBeanBuilder
	 * @param gatewaySenderName
	 * @param propertyName
	 * @param defaultValue
	 */
	private <T extends Object> void configureGatewaySenderFromProperty(BeanDefinitionBuilder gatewaySenderBeanBuilder,
		String gatewaySenderName, String propertyName, String fieldName, T defaultValue) {

		T resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);
		if (resolvedPropertyValue == null) {
			return;
		}

		setPropertyValueOrUseParent(gatewaySenderBeanBuilder, fieldName, resolvedPropertyValue, null, defaultValue);
	}

	private void configureBeanReferenceListFromProperty(BeanDefinitionBuilder gatewaySenderBeanBuilder,
		String gatewaySenderName, String propertyName, String fieldName, String[] defaultValue) {

		String[] resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);
		if (resolvedPropertyValue == null) {
			return;
		}

		setPropertyValueOrUseParentAsBeanReferenceList(gatewaySenderBeanBuilder, fieldName, resolvedPropertyValue,
			new String[] {},
			defaultValue);
	}

	private void configureBeanReferenceFromProperty(BeanDefinitionBuilder gatewaySenderBeanBuilder,
		String gatewaySenderName, String propertyName, String fieldName, String defaultValue) {

		String resolvedPropertyValue = resolveValueFromProperty(gatewaySenderName, propertyName, defaultValue);
		if (resolvedPropertyValue == null) {
			return;
		}

		setPropertyValueIfNotDefaultAsBeanReference(gatewaySenderBeanBuilder, fieldName, resolvedPropertyValue, null,
			defaultValue);
	}

	private List<GatewaySenderConfigurer> resolveGatewaySenderConfigurers() {

		return Optional.ofNullable(this.gatewaySenderConfigurers)
			.filter(gatewaySenderConfigurers -> !gatewaySenderConfigurers.isEmpty())
			.orElseGet(() ->
				Collections.singletonList(LazyResolvingComposableGatewaySenderConfigurer.create(getBeanFactory())));
	}

	private <T extends Object> T resolveValueFromProperty(String gatewaySenderName, String propertyName,
		T defaultValue) {
		Class<T> clazz = (Class<T>) defaultValue.getClass();

		Optional<T> gatewaySenderProperty = Optional
			.ofNullable(resolveProperty(gatewaySenderProperty(propertyName), clazz, null));

		Optional<T> namedGatewaySenderProperty = Optional.ofNullable(
			resolveProperty(namedGatewaySenderProperty(gatewaySenderName, propertyName), clazz, null));

		return namedGatewaySenderProperty.orElse(gatewaySenderProperty.orElse(null));
	}


	private <T> BeanDefinitionBuilder setPropertyValueIfNotDefault(BeanDefinitionBuilder beanDefinitionBuilder,
		String propertyName, T value, T defaultValue) {

		return beanDefinitionBuilder.addPropertyValue(propertyName, Optional.ofNullable(value).orElse(defaultValue));
	}

	private <T> BeanDefinitionBuilder setPropertyValueOrUseParent(BeanDefinitionBuilder beanDefinitionBuilder,
		String propertyName, T value, T parentValue, T defaultValue) {

		T resolveValue = Optional.ofNullable(value).filter(childValue -> !childValue.equals(defaultValue))
			.orElse(Optional.ofNullable(parentValue).orElse(defaultValue));
		return beanDefinitionBuilder.addPropertyValue(propertyName, resolveValue);
	}

	private BeanDefinitionBuilder setPropertyValueIfNotDefaultAsBeanReference(
		BeanDefinitionBuilder beanDefinitionBuilder,
		String propertyName, String value, String parentValue, String defaultValue) {
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
		ManagedList<BeanReference> beanReferences = new ManagedList<>();

		String[] resolvedList = Optional.ofNullable(values).filter(t -> !Arrays.equals(t, defaultList))
			.orElse(Optional.ofNullable(parentValues).orElse(defaultList));
		Arrays.stream(resolvedList)
			.map(RuntimeBeanReference::new)
			.forEach(beanReferences::add);

		return beanDefinitionBuilder.addPropertyValue(propertyName, beanReferences);
	}

	@Override public void setImportMetadata(AnnotationMetadata annotationMetadata) {

	}
}
