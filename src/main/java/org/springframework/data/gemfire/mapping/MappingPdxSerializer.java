/*
 * Copyright 2012-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.mapping;

import java.util.Collections;
import java.util.Map;

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
 * GemFire {@link PdxSerializer} implementation using the Spring Data GemFire {@link GemfireMappingContext}
 * to read and write entities from/to GemFire PDX bytes.
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
 * @see org.springframework.data.mapping.PersistentEntity
 * @see org.springframework.data.mapping.PersistentProperty
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @see org.springframework.data.mapping.model.ConvertingPropertyAccessor
 * @see org.springframework.data.mapping.model.PersistentEntityParameterValueProvider
 * @since 1.2.0
 */
public class MappingPdxSerializer implements PdxSerializer, ApplicationContextAware {

	private final ConversionService conversionService;

	private EntityInstantiators entityInstantiators;

	private final GemfireMappingContext mappingContext;

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private Map<?, PdxSerializer> customPdxSerializers;

	// TODO: decide what to do with this; SpELContext is not used
	private SpELContext context;

	public static MappingPdxSerializer newMappingPdxSerializer() {
		return create(newMappingContext(), newConversionService());
	}

	public static MappingPdxSerializer create(@Nullable ConversionService conversionService) {
		return create(newMappingContext(), conversionService);
	}

	public static MappingPdxSerializer create(@Nullable GemfireMappingContext mappingContext) {
		return create(mappingContext, newConversionService());
	}

	/**
	 * Factory method used to construct a new instance of the {@link MappingPdxSerializer} initialized with
	 * the given {@link GemfireMappingContext mapping context} and {@link ConversionService conversion service}.
	 *
	 * If either the {@link GemfireMappingContext mapping context} or the {@link ConversionService conversion service}
	 * are {@literal null}, then this factory method will provide default instances for each.
	 *
	 * @param mappingContext {@link GemfireMappingContext} used by the {@link MappingPdxSerializer} to map
	 * between application domain object types and PDX serialized bytes based on the entity mapping meta-data.
	 * @param conversionService {@link ConversionService} used by the {@link MappingPdxSerializer} to convert
	 * PDX serialized data to application object property types.
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

		Assert.notNull(mappingContext, "MappingContext is required");
		Assert.notNull(conversionService, "ConversionService is required");

		this.mappingContext = mappingContext;
		this.conversionService = conversionService;
		this.entityInstantiators = new EntityInstantiators();
		this.customPdxSerializers = Collections.emptyMap();
		this.context = new SpELContext(PdxReaderPropertyAccessor.INSTANCE);
	}

	/**
	 * Configures a reference to the Spring {@link ApplicationContext}.
	 *
	 * @param applicationContext reference to the Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.context = new SpELContext(this.context, applicationContext);
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
	public void setCustomPdxSerializers(@NonNull Map<?, PdxSerializer> customPdxSerializers) {

		Assert.notNull(customPdxSerializers, "Custom PdxSerializers are required");

		this.customPdxSerializers = customPdxSerializers;
	}

	/**
	 * @deprecated please use ({@link #setCustomPdxSerializers(Map)} instead.
	 */
	@Deprecated
	public void setCustomSerializers(Map<Class<?>, PdxSerializer> customSerializers) {
		setCustomPdxSerializers(customSerializers);
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
	 * @deprecated please use {@link #getCustomPdxSerializers()} instead.
	 */
	@Deprecated
	@SuppressWarnings("unchecked")
	protected Map<Class<?>, PdxSerializer> getCustomSerializers() {
		return (Map<Class<?>, PdxSerializer>) getCustomPdxSerializers();
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
	protected PdxSerializer getCustomPdxSerializer(@NonNull PersistentProperty<?> property) {

		Map<?, PdxSerializer> customPdxSerializers = getCustomPdxSerializers();

		PdxSerializer customPdxSerializer = customPdxSerializers.get(property);

		customPdxSerializer = customPdxSerializer != null ? customPdxSerializer
			: customPdxSerializers.get(toFullyQualifiedPropertyName(property));

		customPdxSerializer = customPdxSerializer != null ? customPdxSerializer
			: customPdxSerializers.get(property.getType());

		return customPdxSerializer;
	}

	/**
	 * Converts the entity {@link PersistentProperty} to a {@link String fully-qualified property name}.
	 *
	 * @param property {@link PersistentProperty} of the entity.
	 * @return the {@link String fully-qualified property name of the entity {@link PersistentProperty}.
	 * @see org.springframework.data.mapping.PersistentProperty
	 */
	@NonNull
	String toFullyQualifiedPropertyName(@NonNull PersistentProperty<?> property) {
		return property.getOwner().getType().getName().concat(".").concat(property.getName());
	}

	/**
	 * @deprecated please use {@link #getCustomPdxSerializer(PersistentProperty)} instead.
	 */
	@Nullable
	@Deprecated
	protected PdxSerializer getCustomSerializer(Class<?> type) {
		return getCustomPdxSerializers().get(type);
	}

	/**
	 * Configures the {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}.
	 *
	 * @param entityInstantiators {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}; must not be {@literal null}.
	 * @see org.springframework.data.convert.EntityInstantiator
	 */
	public void setGemfireInstantiators(@NonNull EntityInstantiators entityInstantiators) {

		Assert.notNull(entityInstantiators, "EntityInstantiators are required");

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
	public void setGemfireInstantiators(@NonNull Map<Class<?>, EntityInstantiator> gemfireInstantiators) {
		setGemfireInstantiators(new EntityInstantiators(gemfireInstantiators));
	}

	/**
	 * Returns the configured {@link EntityInstantiators} handling instantiation for GemFire persistent entities.
	 *
	 * @return the configured {@link EntityInstantiators} handling instantiation for GemFire persistent entities.
	 * @see org.springframework.data.convert.EntityInstantiators
	 */
	protected EntityInstantiators getGemfireInstantiators() {
		return this.entityInstantiators;
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
	protected EntityInstantiator getInstantiatorFor(PersistentEntity entity) {
		return getGemfireInstantiators().getInstantiatorFor(entity);
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
	 * logic between GemFire persistent entities and application domain object {@link Class types}.
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
	protected GemfirePersistentEntity<?> getPersistentEntity(Class<?> entityType) {
		return getMappingContext().getPersistentEntity(entityType);
	}

	@Override
	public Object fromData(Class<?> type, PdxReader reader) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(type);

		Object instance = getInstantiatorFor(entity)
			.createInstance(entity, new PersistentEntityParameterValueProvider<>(entity,
				new GemfirePropertyValueProvider(reader), null));

		PersistentPropertyAccessor propertyAccessor =
			new ConvertingPropertyAccessor(entity.getPropertyAccessor(instance), getConversionService());

		entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

			if (isWritable(entity, persistentProperty)) {

				PdxSerializer customPdxSerializer = getCustomPdxSerializer(persistentProperty);

				Object value = null;

				try {
					if (getLogger().isDebugEnabled()) {
						getLogger().debug(String.format("Setting property [%1$s] for entity [%2$s] of type [%3$s] from PDX%4$s",
							persistentProperty.getName(), instance, type, (customPdxSerializer != null ?
								String.format(" using custom PdxSerializer [%1$s]", customPdxSerializer) : "")));
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
					throw new MappingException(String.format(
						"While setting value [%1$s] of property [%2$s] for entity of type [%3$s] from PDX%4$s",
							value, persistentProperty.getName(), type, (customPdxSerializer != null ?
								String.format(" using custom PdxSerializer [%1$s]", customPdxSerializer) : "")), cause);
				}
			}
		});

		return propertyAccessor.getBean();
	}

	/* (non-Javadoc) */
	boolean isWritable(GemfirePersistentEntity<?> entity, GemfirePersistentProperty persistentProperty) {

		return !entity.isConstructorArgument(persistentProperty)
			&& persistentProperty.isWritable()
			&& !persistentProperty.isTransient();
	}

	@Override
	@SuppressWarnings("unchecked")
	public boolean toData(Object value, PdxWriter writer) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(value);

		// Entity will be null for simple types
		if (entity != null) {

			PersistentPropertyAccessor propertyAccessor =
				new ConvertingPropertyAccessor(entity.getPropertyAccessor(value), getConversionService());

			entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

				if (isReadable(persistentProperty)) {

					PdxSerializer customPdxSerializer = getCustomPdxSerializer(persistentProperty);

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

	/* (non-Javadoc) */
	boolean isReadable(GemfirePersistentProperty persistentProperty) {
		return !persistentProperty.isTransient();
	}
}
