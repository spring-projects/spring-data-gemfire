/*
 * Copyright 2012-2020 the original author or authors.
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
package org.springframework.data.gemfire.mapping;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.function.Predicate;

import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.data.convert.EntityInstantiator;
import org.springframework.data.convert.EntityInstantiators;
import org.springframework.data.gemfire.util.Filter;
import org.springframework.data.mapping.MappingException;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PersistentProperty;
import org.springframework.data.mapping.PersistentPropertyAccessor;
import org.springframework.data.mapping.PropertyHandler;
import org.springframework.data.mapping.model.ConvertingPropertyAccessor;
import org.springframework.data.mapping.model.PersistentEntityParameterValueProvider;
import org.springframework.data.mapping.model.SpELContext;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * Pivotal GemFire {@link PdxSerializer} implementation using the SDG {@link GemfireMappingContext} to read and write entities
 * from/to Pivotal GemFire PDX bytes.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.pdx.PdxReader
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.PdxWriter
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.core.convert.ConversionService
 * @see org.springframework.data.convert.EntityInstantiator
 * @see org.springframework.data.convert.EntityInstantiators
 * @see org.springframework.data.gemfire.util.Filter
 * @see org.springframework.data.mapping.PersistentEntity
 * @see org.springframework.data.mapping.PersistentProperty
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @see org.springframework.data.mapping.model.ConvertingPropertyAccessor
 * @see org.springframework.data.mapping.model.PersistentEntityParameterValueProvider
 * @since 1.2.0
 */
public class MappingPdxSerializer implements PdxSerializer, ApplicationContextAware {

	protected static final String JAVA_PACKAGE_NAME = "java";
	protected static final String COM_GEMSTONE_GEMFIRE_PACKAGE_NAME = "com.gemstone.gemfire";
	protected static final String ORG_APACHE_GEODE_PACKAGE_NAME = "org.apache.geode";
	protected static final String ORG_SPRINGFRAMEWORK_PACKAGE_NAME = "org.springframework";

	/**
	 * Factory method used to construct a new instance of {@link MappingPdxSerializer} initialized with
	 * a provided {@link GemfireMappingContext} and default {@link ConversionService}.
	 *
	 * @return a new instance of {@link MappingPdxSerializer}.
	 * @see #create(GemfireMappingContext, ConversionService)
	 * @see #newMappingContext()
	 * @see #newConversionService()
	 */
	public static MappingPdxSerializer newMappingPdxSerializer() {
		return create(newMappingContext(), newConversionService());
	}

	/**
	 * Factory method used to construct a new instance of {@link MappingPdxSerializer} initialized with
	 * the given {@link ConversionService} and a provided {@link GemfireMappingContext}.
	 *
	 * @param conversionService {@link ConversionService} used to convert persistent values to entity properties.
	 * @return a new instance of {@link MappingPdxSerializer} initialized with the given {@link ConversionService}.
	 * @see org.springframework.core.convert.ConversionService
	 * @see #create(GemfireMappingContext, ConversionService)
	 * @see #newMappingContext()
	 */
	public static MappingPdxSerializer create(@Nullable ConversionService conversionService) {
		return create(newMappingContext(), conversionService);
	}

	/**
	 * Factory method used to construct a new instance of {@link MappingPdxSerializer} initialized with
	 * the given {@link GemfireMappingContext mapping context} supplying entity mapping meta-data,
	 * using a provided, default {@link ConversionService}.
	 *
	 * @param mappingContext {@link GemfireMappingContext} used to supply entity mapping meta-data.
	 * @return a new instance of {@link MappingPdxSerializer} initialized with
	 * the given {@link GemfireMappingContext mapping context}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see #create(GemfireMappingContext, ConversionService)
	 * @see #newConversionService()
	 */
	public static MappingPdxSerializer create(@Nullable GemfireMappingContext mappingContext) {
		return create(mappingContext, newConversionService());
	}

	/**
	 * Factory method used to construct a new instance of {@link MappingPdxSerializer} initialized with
	 * the given {@link GemfireMappingContext mapping context} and {@link ConversionService conversion service}.
	 *
	 * If either the {@link GemfireMappingContext mapping context} or the {@link ConversionService conversion service}
	 * are {@literal null}, then this factory method will provide default instances for each.
	 *
	 * @param mappingContext {@link GemfireMappingContext} used to map between application domain model object types
	 * and PDX serialized bytes based on the entity's mapping meta-data.
	 * @param conversionService {@link ConversionService} used to convert persistent values to entity properties.
	 * @return an initialized instance of the {@link MappingPdxSerializer}.
	 * @see org.springframework.core.convert.ConversionService
	 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
	 */
	public static MappingPdxSerializer create(@Nullable GemfireMappingContext mappingContext,
			@Nullable ConversionService conversionService) {

		return new MappingPdxSerializer(
			resolveMappingContext(mappingContext),
			resolveConversionService(conversionService)
		);
	}

	/**
	 * Constructs a new {@link ConversionService}.
	 *
	 * @return a new {@link ConversionService}.
	 * @see org.springframework.core.convert.ConversionService
	 */
	private static ConversionService newConversionService() {
		return new DefaultConversionService();
	}

	/**
	 * Resolves the {@link ConversionService} used for conversions.
	 *
	 * @param conversionService {@link ConversionService} to evaluate.
	 * @return the given {@link ConversionService} if not {@literal null} or a new {@link ConversionService}.
	 * @see org.springframework.core.convert.ConversionService
	 * @see #newConversionService()
	 */
	private static ConversionService resolveConversionService(ConversionService conversionService) {
		return conversionService != null ? conversionService : newConversionService();
	}

	/**
	 * Constructs a new {@link GemfireMappingContext}.
	 *
	 * @return a new {@link GemfireMappingContext}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 */
	private static GemfireMappingContext newMappingContext() {
		return new GemfireMappingContext();
	}

	/**
	 * Resolves the {@link GemfireMappingContext mapping context} used to provide mapping meta-data.
	 *
	 * @param mappingContext {@link GemfireMappingContext} to evaluate.
	 * @return the given {@link GemfireMappingContext mapping context} if not {@literal null}
	 * or a new {@link GemfireMappingContext mapping context}.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 * @see #newMappingContext()
	 */
	private static GemfireMappingContext resolveMappingContext(GemfireMappingContext mappingContext) {
		return mappingContext != null ? mappingContext : newMappingContext();
	}

	private final ConversionService conversionService;

	private EntityInstantiators entityInstantiators;

	private final GemfireMappingContext mappingContext;

	private final List<PdxSerializerResolver> pdxSerializerResolvers = new CopyOnWriteArrayList<>();

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private final Map<Object, PdxSerializer> customPdxSerializers = new ConcurrentHashMap<>();

	private Predicate<Class<?>> excludeTypeFilters = TypeFilters.EXCLUDE_NULL_TYPES
		.and(TypeFilters.EXCLUDE_JAVA_TYPES)
		.and(TypeFilters.EXCLUDE_COM_GEMSTONE_GEMFIRE_TYPES)
		.and(TypeFilters.EXCLUDE_ORG_APACHE_GEODE_TYPES)
		.and(TypeFilters.EXCLUDE_ORG_SPRINGFRAMEWORK_TYPES);

	private Predicate<Class<?>> includeTypeFilters = TypeFilters.EXCLUDE_ALL_TYPES;

	// TODO remove? SpELContext is not used
	private SpELContext spelContext;

	/**
	 * Constructs a new instance of {@link MappingPdxSerializer} using a default {@link GemfireMappingContext}
	 * and {@link DefaultConversionService}.
	 *
	 * @see #newConversionService()
	 * @see #newMappingContext()
	 * @see org.springframework.core.convert.support.DefaultConversionService
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 */
	public MappingPdxSerializer() {
		this(newMappingContext(), newConversionService());
	}

	/**
	 * Constructs a new instance of {@link MappingPdxSerializer} initialized with the given
	 * {@link GemfireMappingContext} and {@link ConversionService}.
	 *
	 * @param mappingContext {@link GemfireMappingContext} used by the {@link MappingPdxSerializer} to map
	 * between application domain object types and PDX serialized bytes based on the entity mapping meta-data.
	 * @param conversionService {@link ConversionService} used by the {@link MappingPdxSerializer} to convert
	 * PDX serialized data to application object property types.
	 * @throws IllegalArgumentException if either the {@link GemfireMappingContext} or the {@link ConversionService}
	 * is {@literal null}.
	 */
	public MappingPdxSerializer(GemfireMappingContext mappingContext, ConversionService conversionService) {

		Assert.notNull(mappingContext, "MappingContext must not be null");
		Assert.notNull(conversionService, "ConversionService must not be null");

		this.mappingContext = mappingContext;
		this.conversionService = conversionService;
		this.entityInstantiators = new EntityInstantiators();

		this.pdxSerializerResolvers.addAll(Arrays.asList(
			PdxSerializerResolvers.PROPERTY,
			PdxSerializerResolvers.PROPERTY_NAME,
			PdxSerializerResolvers.PROPERTY_TYPE
		));

		this.spelContext = new SpELContext(PdxReaderPropertyAccessor.INSTANCE);
	}

	/**
	 * Configures a reference to the Spring {@link ApplicationContext}.
	 *
	 * @param applicationContext reference to the Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.spelContext = new SpELContext(this.spelContext, applicationContext);
	}

	/**
	 * Returns a reference to the configured {@link ConversionService} used to convert data store types
	 * to application domain object types.
	 *
	 * @return a reference to the configured {@link ConversionService}.
	 * @see org.springframework.core.convert.ConversionService
	 */
	@NonNull
	protected ConversionService getConversionService() {
		return this.conversionService;
	}

	/**
	 * Configures custom {@link PdxSerializer PDX serializers} used to customize the serialization for specific
	 * application {@link Class domain types}.
	 *
	 * @param customPdxSerializers {@link Map mapping} containing custom {@link PdxSerializer PDX serializers}
	 * used to customize the serialization of specific application {@link Class domain types}.
	 * @throws IllegalArgumentException if the {@link Map custom PDX serializer mapping} is {@literal null}.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see java.util.Map
	 */
	public void setCustomPdxSerializers(Map<?, PdxSerializer> customPdxSerializers) {
		Optional.ofNullable(customPdxSerializers).ifPresent(this.customPdxSerializers::putAll);
	}

	/**
	 * Returns a {@link Map mapping} of application {@link Class domain types} to custom
	 * {@link PdxSerializer PDX serializers} used to customize the serialization
	 * for specific application {@link Class domain types}.
	 *
	 * @return a {@link Map mapping} of application {@link Class domain types}
	 * to custom {@link PdxSerializer PDX serializers}.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see java.util.Map
	 */
	@NonNull
	protected Map<?, PdxSerializer> getCustomPdxSerializers() {
		return Collections.unmodifiableMap(this.customPdxSerializers);
	}

	/**
	 * Configures the {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}.
	 *
	 * @param entityInstantiators {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}; must not be {@literal null}.
	 * @see org.springframework.data.convert.EntityInstantiator
	 */
	public void setEntityInstantiators(@NonNull EntityInstantiators entityInstantiators) {

		Assert.notNull(entityInstantiators, "EntityInstantiators must not be null");

		this.entityInstantiators = entityInstantiators;
	}

	/**
	 * Configures the {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}.
	 *
	 * @param gemfireInstantiators mapping of {@link Class types} to {@link EntityInstantiator} objects;
	 * must not be {@literal null}.
	 * @see org.springframework.data.convert.EntityInstantiator
	 * @see java.util.Map
	 */
	public void setEntityInstantiators(@NonNull Map<Class<?>, EntityInstantiator> gemfireInstantiators) {
		setEntityInstantiators(new EntityInstantiators(gemfireInstantiators));
	}

	/**
	 * Returns the configured {@link EntityInstantiators} handling instantiation for Pivotal GemFire persistent entities.
	 *
	 * @return the configured {@link EntityInstantiators} handling instantiation for Pivotal GemFire persistent entities.
	 * @see org.springframework.data.convert.EntityInstantiators
	 */
	protected EntityInstantiators getEntityInstantiators() {
		return this.entityInstantiators;
	}

	/**
	 * Returns a reference to the configured {@link Logger} used to log {@link String messages}
	 * about the functions of this {@link PdxSerializer}.
	 *
	 * @return a reference to the configured {@link Logger}.
	 * @see org.slf4j.Logger
	 */
	@NonNull
	protected Logger getLogger() {
		return this.logger;
	}

	/**
	 * Returns a reference to the configured {@link GemfireMappingContext mapping context} used to handling mapping
	 * logic between Pivotal GemFire persistent entities and application domain object {@link Class types}.
	 *
	 * @return a reference to the configured {@link GemfireMappingContext mapping context} for Pivotal GemFire.
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 */
	@NonNull
	protected GemfireMappingContext getMappingContext() {
		return this.mappingContext;
	}

	/**
	 * Looks up and returns the {@link PersistentEntity} meta-data for the given entity object.
	 *
	 * @param entity actual persistent entity, application domain object.
	 * @return the {@link PersistentEntity} meta-data for the given entity object.
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
	 * @see #getPersistentEntity(Class)
	 */
	protected GemfirePersistentEntity<?> getPersistentEntity(@NonNull Object entity) {
		return getPersistentEntity(entity.getClass());
	}

	/**
	 * Looks up and returns the {@link PersistentEntity} meta-data for the given entity {@link Class} type.
	 *
	 * @param entityType {@link Class} type of the actual persistent entity, application domain object {@link Class}.
	 * @return the {@link PersistentEntity} meta-data for the given entity {@link Class} type.
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
	 * @see #getMappingContext()
	 */
	protected GemfirePersistentEntity<?> getPersistentEntity(@NonNull Class<?> entityType) {
		return getMappingContext().getPersistentEntity(entityType);
	}

	/**
	 * Sets the {@link Predicate type filters} used to exclude (a.k.a. filter) {@link Class types} serializable
	 * by this {@link MappingPdxSerializer PDX serializer}.
	 *
	 * This operation is null-safe and rather than overriding the existing {@link Predicate excluded type filters},
	 * this set operation combines the given {@link Predicate exclude type filters} with
	 * the exiting {@link Predicate excluded type filters} joined by {@literal and}.
	 *
	 * @param excludeTypeFilters {@link Predicate type filters} used to exclude/filter {@link Class types} serializable
	 * by this {@link MappingPdxSerializer PDX serializer}.
	 * @see java.util.function.Predicate
	 */
	public void setExcludeTypeFilters(@Nullable Predicate<Class<?>> excludeTypeFilters) {

		this.excludeTypeFilters = excludeTypeFilters != null
			? this.excludeTypeFilters.and(excludeTypeFilters)
			: this.excludeTypeFilters;
	}

	/**
	 * Sets the {@link Predicate type filters} used to include {@link Class types} serializable
	 * by this {@link MappingPdxSerializer PDX serializer}.
	 *
	 * This operation is null-safe and rather than overriding the existing {@link Predicate included type filters},
	 * this set operation combines the given {@link Predicate include type filters} with
	 * the exiting {@link Predicate included type filters} joined by {@literal or}.
	 *
	 * @param includeTypeFilters {@link Predicate type filters} used to include {@link Class types} serializable
	 * by this {@link MappingPdxSerializer PDX serializer}.
	 * @see java.util.function.Predicate
	 */
	public void setIncludeTypeFilters(@Nullable Predicate<Class<?>> includeTypeFilters) {

		this.includeTypeFilters = includeTypeFilters != null
			? this.includeTypeFilters.or(includeTypeFilters)
			: this.includeTypeFilters;
	}

	/**
	 * Returns the {@link Predicate type filters} used to filter {@link Class types} serializable
	 * by this {@link MappingPdxSerializer PdxSerializer}.
	 *
	 * @return the resolved {@link Predicate type filter}.
	 * @see java.util.function.Predicate
	 */
	protected Predicate<Class<?>> getTypeFilters() {
		return this.excludeTypeFilters.or(TypeFilters.EXCLUDE_NULL_TYPES.and(this.includeTypeFilters));
	}

	/**
	 * Registers the given {@link PdxSerializerResolver}, which will be used to resolve a custom {@link PdxSerializer}
	 * for a entity property.
	 *
	 * The strategy, or criteria used to resolve the custom {@link PdxSerializer} is up to the individual resolve
	 * and can be based on things like the property type, or fully-qualified property name, etc.
	 *
	 * @param pdxSerializerResolver {@link PdxSerializerResolver} used to resolve a custom {@link PdxSerializer}
	 * for a entity property.
	 */
	public void register(PdxSerializerResolver pdxSerializerResolver) {
		Optional.ofNullable(pdxSerializerResolver).ifPresent(it -> this.pdxSerializerResolvers.add(0, it));
	}

	@Override
	public Object fromData(Class<?> type, PdxReader reader) {
		return getTypeFilters().test(type) ? doFromData(type, reader) : null;
	}

	/**
	 * Converts a set of PDX serialized bytes to an {@link Object} of the specified {@link Class type}.
	 *
	 * @param type desired {@link Class type} of the {@link Object}.
	 * @param reader {@link PdxReader} used to access the PDX bytes to convert.
	 * @return an {@link Object} of the specified {@link Class type} converted from the PDX bytes.
	 * @see org.apache.geode.pdx.PdxReader
	 * @see java.lang.Object
	 * @see java.lang.Class
	 */
	@SuppressWarnings("unchecked")
	Object doFromData(Class<?> type, PdxReader reader) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(type);

		Object instance = resolveEntityInstantiator(entity)
			.createInstance(entity, new PersistentEntityParameterValueProvider<>(entity,
				new GemfirePropertyValueProvider(reader), null));

		PersistentPropertyAccessor propertyAccessor =
			new ConvertingPropertyAccessor(entity.getPropertyAccessor(instance), getConversionService());

		entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

			if (isWritable(entity, persistentProperty)) {

				PdxSerializer customPdxSerializer = resolveCustomPdxSerializer(persistentProperty);

				Object value = null;

				try {
					if (getLogger().isDebugEnabled()) {
						getLogger().debug(String.format("Setting property [%1$s] for entity [%2$s] of type [%3$s] from PDX%4$s",
							persistentProperty.getName(), instance, type, (customPdxSerializer != null
								? String.format(" using custom PdxSerializer [%s]", customPdxSerializer) : "")));
					}

					value = (customPdxSerializer != null
						? customPdxSerializer.fromData(persistentProperty.getType(), reader)
						: reader.readField(persistentProperty.getName()));

					if (getLogger().isDebugEnabled()) {
						getLogger().debug(String.format("... with value [%s]", value));
					}

					propertyAccessor.setProperty(persistentProperty, value);
				}
				catch (Exception cause) {
					throw new MappingException(
						String.format("While setting value [%1$s] of property [%2$s] for entity of type [%3$s] from PDX%4$s",
						value, persistentProperty.getName(), type, (customPdxSerializer != null
							? String.format(" using custom PdxSerializer [%s]", customPdxSerializer) : "")), cause);
				}
			}
		});

		return propertyAccessor.getBean();
	}

	/**
	 * Determines whether the {@link PersistentProperty persistent property}
	 * of the given {@link PersistentEntity entity } is writable.
	 *
	 * The {@link PersistentProperty persistent property} is considered {@literal writable} if the property
	 * is not a constructor parameter of the {@link PersistentEntity entity's} {@link Class type}, the property
	 * has a {@literal setter} method and the property is not {@literal transient}.
	 *
	 * @param entity {@link GemfirePersistentEntity} containing the {@link GemfirePersistentProperty property}.
	 * @param persistentProperty {@link GemfirePersistentProperty} to evaluate.
	 * @return a boolean value indicating whether the {@link PersistentProperty persistent property}
	 * of the given {@link PersistentEntity entity } is writable.
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentEntity
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
	 */
	boolean isWritable(GemfirePersistentEntity<?> entity, GemfirePersistentProperty persistentProperty) {

		return !entity.isConstructorArgument(persistentProperty)
			&& persistentProperty.isWritable()
			&& !persistentProperty.isTransient();
	}

	@Override
	public boolean toData(Object value, PdxWriter writer) {
		return getTypeFilters().test(resolveType(value)) && doToData(value, writer);
	}

	/**
	 * Converts the given {@link Object} into a stream of PDX bytes.
	 *
	 * @param value {@link Object} to convert.
	 * @param writer {@link PdxWriter} used to stream the given {@link Object} into a stream of PDX bytes.
	 * @return a boolean value indicating whether this {@link MappingPdxSerializer PDX serializer} was able to
	 * write the given {@link Object} as a stream of PDX bytes.
	 * @see org.apache.geode.pdx.PdxWriter
	 * @see java.lang.Object
	 */
	@SuppressWarnings("unchecked")
	boolean doToData(Object value, PdxWriter writer) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(value);

		// Entity will be null for simple types (e.g. int, Long, String, etc).
		if (entity != null) {

			PersistentPropertyAccessor propertyAccessor =
				new ConvertingPropertyAccessor(entity.getPropertyAccessor(value), getConversionService());

			entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

				if (isReadable(persistentProperty)) {

					PdxSerializer customPdxSerializer = resolveCustomPdxSerializer(persistentProperty);

					Object propertyValue = null;

					try {

						propertyValue = propertyAccessor.getProperty(persistentProperty);

						if (getLogger().isDebugEnabled()) {
							getLogger().debug(String.format("Serializing entity [%1$s] property [%2$s] value [%3$s] of type [%4$s] to PDX%5$s",
								entity.getType().getName(), persistentProperty.getName(), propertyValue,
								ObjectUtils.nullSafeClassName(propertyValue), (customPdxSerializer != null
									? String.format(" using custom PdxSerializer [%s]", customPdxSerializer) : "")));
						}

						if (customPdxSerializer != null) {
							customPdxSerializer.toData(propertyValue, writer);
						}
						else {
							writer.writeField(persistentProperty.getName(), propertyValue,
								(Class<Object>) persistentProperty.getType());
						}
					}
					catch (Exception cause) {
						throw new MappingException(String.format(
							"While serializing entity [%1$s] property [%2$s] value [%3$s] of type [%4$s] to PDX%5$s",
							entity.getType().getName(), persistentProperty.getName(), propertyValue,
							ObjectUtils.nullSafeClassName(propertyValue), (customPdxSerializer != null
								? String.format(" using custom PdxSerializer [%1$s].",
									customPdxSerializer.getClass().getName()) : "")), cause);
					}
				}
			});

			GemfirePersistentProperty idProperty = entity.getIdProperty();

			if (idProperty != null) {
				writer.markIdentityField(idProperty.getName());
			}

			return true;
		}
		return false;
	}

	/**
	 * Determines whether the given {@link PersistentProperty persistent property} is readable.
	 *
	 * The {@link PersistentProperty persistent property} is considered {@literal readable}
	 * if the property is not {@literal transient}.
	 *
	 * @param persistentProperty {@link GemfirePersistentProperty} to evaluate.
	 * @return a boolean value indicating whether the {@link PersistentProperty persistent property}
	 * is readable.
	 * @see org.springframework.data.gemfire.mapping.GemfirePersistentProperty
	 */
	boolean isReadable(GemfirePersistentProperty persistentProperty) {
		return !persistentProperty.isTransient();
	}

	/**
	 * Returns a custom PDX serializer for the given {@link PersistentProperty entity persistent property}.
	 *
	 * @param property {@link PersistentProperty} of the entity used to lookup the custom PDX serializer.
	 * @return a custom {@link PdxSerializer} for the given entity {@link PersistentProperty},
	 * or {@literal null} if no custom {@link PdxSerializer} could be found.
	 * @see org.apache.geode.pdx.PdxSerializer
	 */
	@Nullable
	protected PdxSerializer resolveCustomPdxSerializer(@NonNull PersistentProperty<?> property) {

		Map<?, PdxSerializer> customPdxSerializers = getCustomPdxSerializers();

		return this.pdxSerializerResolvers.stream()
			.map(it -> it.resolve(customPdxSerializers, property))
			.filter(Objects::nonNull)
			.findFirst()
			.orElse(null);
	}

	/**
	 * Looks up and returns an EntityInstantiator to construct and initialize an instance of the object defined
	 * by the given PersistentEntity (meta-data).
	 *
	 * @param entity the PersistentEntity object used to lookup the custom EntityInstantiator.
	 * @return an EntityInstantiator for the given PersistentEntity.
	 * @see org.springframework.data.convert.EntityInstantiator
	 * @see org.springframework.data.mapping.PersistentEntity
	 */
	protected EntityInstantiator resolveEntityInstantiator(PersistentEntity entity) {
		return getEntityInstantiators().getInstantiatorFor(entity);
	}

	/**
	 * Resolves the {@link Class type} of the given {@link Object}.
	 *
	 * @param obj {@link Object} to evaluate.
	 * @return the {@link Class type} of the given {@link Object}.
	 * @see java.lang.Object#getClass()
	 * @see java.lang.Class
	 */
	@Nullable
	Class<?> resolveType(@Nullable Object obj) {
		return obj != null ? obj.getClass() : null;
	}

	@FunctionalInterface
	public interface PdxSerializerResolver {

		@Nullable
		PdxSerializer resolve(@NonNull Map<?, PdxSerializer> customPdxSerializers,
			@NonNull PersistentProperty<?> property);

	}

	public enum PdxSerializerResolvers implements PdxSerializerResolver {

		PROPERTY {

			@Override
			public PdxSerializer resolve(Map<?, PdxSerializer> customPdxSerializers, PersistentProperty<?> property) {
				return customPdxSerializers.get(property);
			}
		},

		PROPERTY_NAME {

			@Override
			public PdxSerializer resolve(Map<?, PdxSerializer> customPdxSerializers, PersistentProperty<?> property) {
				return customPdxSerializers.get(toFullyQualifiedPropertyName(property));
			}
		},

		PROPERTY_TYPE {

			@Override
			public PdxSerializer resolve(Map<?, PdxSerializer> customPdxSerializers, PersistentProperty<?> property) {
				return customPdxSerializers.get(property.getType());
			}
		};

		/**
		 * Converts the entity {@link PersistentProperty} to a {@link String fully-qualified property name}.
		 *
		 * @param property {@link PersistentProperty} of the entity.
		 * @return the {@link String fully-qualified property name of the entity {@link PersistentProperty}.
		 * @see org.springframework.data.mapping.PersistentProperty
		 */
		@NonNull
		static String toFullyQualifiedPropertyName(@NonNull PersistentProperty<?> property) {
			return property.getOwner().getType().getName().concat(".").concat(property.getName());
		}
	}

	public enum TypeFilters implements Filter<Class<?>> {

		EXCLUDE_ALL_TYPES {

			@Override
			public boolean accept(@Nullable Class<?> type) {
				return false;
			}
		},

		EXCLUDE_JAVA_TYPES {

			@Override
			public boolean accept(@Nullable Class<?> type) {

				return Optional.ofNullable(type)
					.filter(it -> !it.getPackage().getName().startsWith(JAVA_PACKAGE_NAME))
					.isPresent();
			}
		},

		EXCLUDE_NULL_TYPES {

			@Override
			public boolean accept(@Nullable Class<?> type) {
				return type != null;
			}
		},

		EXCLUDE_COM_GEMSTONE_GEMFIRE_TYPES {

			@Override
			public boolean accept(@Nullable Class<?> type) {

				return Optional.ofNullable(type)
					.filter(it -> !it.getPackage().getName().startsWith(COM_GEMSTONE_GEMFIRE_PACKAGE_NAME))
					.isPresent();
			}
		},

		EXCLUDE_ORG_APACHE_GEODE_TYPES {

			@Override
			public boolean accept(Class<?> type) {

				return Optional.ofNullable(type)
					.filter(it -> !it.getPackage().getName().startsWith(ORG_APACHE_GEODE_PACKAGE_NAME))
					.isPresent();
			}
		},

		EXCLUDE_ORG_SPRINGFRAMEWORK_TYPES {

			@Override
			public boolean accept(@Nullable Class<?> type) {

				return Optional.ofNullable(type)
					.filter(it -> !it.getPackage().getName().startsWith(ORG_SPRINGFRAMEWORK_PACKAGE_NAME))
					.isPresent();
			}
		},
	}
}
