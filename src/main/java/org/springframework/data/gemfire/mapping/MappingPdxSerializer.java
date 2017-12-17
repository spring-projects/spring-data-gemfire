/*
 * Copyright 2012-2015 the original author or authors.
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
import org.springframework.data.mapping.PersistentPropertyAccessor;
import org.springframework.data.mapping.PropertyHandler;
import org.springframework.data.mapping.model.ConvertingPropertyAccessor;
import org.springframework.data.mapping.model.PersistentEntityParameterValueProvider;
import org.springframework.data.mapping.model.SpELContext;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * GemFire {@link PdxSerializer} implementation that uses a Spring Data GemFire {@link GemfireMappingContext}
 * to read and write entities.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.core.convert.ConversionService
 * @see org.springframework.data.convert.EntityInstantiator
 * @see org.springframework.data.convert.EntityInstantiators
 * @see org.springframework.data.mapping.PersistentEntity
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @see org.springframework.data.mapping.model.ConvertingPropertyAccessor
 * @see org.apache.geode.pdx.PdxReader
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.PdxWriter
 * @since 1.2.0
 */
public class MappingPdxSerializer implements PdxSerializer, ApplicationContextAware {

	private final ConversionService conversionService;

	private EntityInstantiators entityInstantiators;

	private final GemfireMappingContext mappingContext;

	private final Logger logger = LoggerFactory.getLogger(getClass());

	private Map<Class<?>, PdxSerializer> customSerializers;

	// TODO: decide what to do with this; the SpELContext is not used
	private SpELContext context;

	/**
	 * Factory method to construct a new instance of the {@link MappingPdxSerializer} initialized with the given
	 * {@link GemfireMappingContext} and Spring {@link ConversionService}.  If either the {@link GemfireMappingContext}
	 * or Spring {@link ConversionService} are {@literal null}, then this factory method will construct default
	 * instances of each.
	 *
	 * @param mappingContext {@link GemfireMappingContext} used by this {@link PdxSerializer} to handle mappings
	 * between application domain object types and PDX Serialization meta-data/data.
	 * @param conversionService Spring's {@link ConversionService} used to convert PDX deserialized data to application
	 * object property types.
	 * @return an initialized instance of the {@link MappingPdxSerializer}.
	 * @see org.springframework.core.convert.ConversionService
	 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
	 */
	public static MappingPdxSerializer create(GemfireMappingContext mappingContext,
			ConversionService conversionService) {

		mappingContext = mappingContext != null ? mappingContext : new GemfireMappingContext();
		conversionService = conversionService != null ? conversionService : new DefaultConversionService();

		return new MappingPdxSerializer(mappingContext, conversionService);
	}

	/**
	 * Creates a new {@link MappingPdxSerializer} using the default {@link GemfireMappingContext}
	 * and {@link DefaultConversionService}.
	 *
	 * @see org.springframework.core.convert.support.DefaultConversionService
	 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
	 */
	public MappingPdxSerializer() {
		this(new GemfireMappingContext(), new DefaultConversionService());
	}

	/**
	 * Creates a new {@link MappingPdxSerializer} using the given
	 * {@link GemfireMappingContext} and {@link ConversionService}.
	 *
	 * @param mappingContext must not be {@literal null}.
	 * @param conversionService must not be {@literal null}.
	 */
	public MappingPdxSerializer(GemfireMappingContext mappingContext, ConversionService conversionService) {

		Assert.notNull(mappingContext, "MappingContext is required");
		Assert.notNull(conversionService, "ConversionService is required");

		this.mappingContext = mappingContext;
		this.conversionService = conversionService;
		this.entityInstantiators = new EntityInstantiators();
		this.customSerializers = Collections.emptyMap();
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
	 * @return a refernce to the configured {@link ConversionService}.
	 * @see org.springframework.core.convert.ConversionService
	 */
	protected ConversionService getConversionService() {
		return this.conversionService;
	}

	/**
	 * Configures custom {@link PdxSerializer PDX serializers} used to serialize
	 * specific application {@link Class class types}.
	 *
	 * @param customSerializers mapping of application domain object {@link Class class types}
	 * to {@link PdxSerializer PDX serializers} used to handle those {@link Class class types}.
	 * @throws IllegalArgumentException if the {@link Map} is {@literal null}.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see java.util.Map
	 */
	public void setCustomSerializers(Map<Class<?>, PdxSerializer> customSerializers) {

		Assert.notNull(customSerializers, "Custom PdxSerializers are required");

		this.customSerializers = customSerializers;
	}

	/**
	 * Returns a {@link Map mapping} of application domain object {@link Class types}
	 * to custom {@link PdxSerializer PdxSerializers} used to handle custom serialization logic
	 * for the application domain objects.
	 *
	 * @return a {@link Map mapping} of application domain object {@link Class types}
	 * to custom {@link PdxSerializer PdxSerializers}.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see java.util.Map
	 */
	protected Map<Class<?>, PdxSerializer> getCustomSerializers() {
		return Collections.unmodifiableMap(this.customSerializers);
	}

	/**
	 * Looks up and returns a custom PdxSerializer based on the class type of the object to (de)serialize.
	 *
	 * @param type the Class type of the object to (de)serialize.
	 * @return a "custom" PdxSerializer for the given class type or null if no custom PdxSerializer
	 * for the given class type was registered.
	 * @see #getCustomSerializers()
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see java.lang.Class
	 */
	protected PdxSerializer getCustomSerializer(Class<?> type) {
		return getCustomSerializers().get(type);
	}

	/**
	 * Configures the {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}.
	 *
	 * @param entityInstantiators {@link EntityInstantiator EntityInstantiators} used to create the instances
	 * read by this {@link PdxSerializer}; must not be {@literal null}.
	 * @see org.springframework.data.convert.EntityInstantiator
	 */
	public void setGemfireInstantiators(EntityInstantiators entityInstantiators) {

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
	public void setGemfireInstantiators(Map<Class<?>, EntityInstantiator> gemfireInstantiators) {
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
	protected GemfirePersistentEntity<?> getPersistentEntity(Object entity) {
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

				PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType());

				Object value = null;

				try {
					if (getLogger().isDebugEnabled()) {
						getLogger().debug(String.format("Setting property [%1$s] for entity [%2$s] of type [%3$s] from PDX%4$s",
							persistentProperty.getName(), instance, type, (customSerializer != null ?
								String.format(" using custom PdxSerializer [%1$s]", customSerializer) : "")));
					}

					value = (customSerializer != null
						? customSerializer.fromData(persistentProperty.getType(), reader)
						: reader.readField(persistentProperty.getName()));

					if (getLogger().isDebugEnabled()) {
						getLogger().debug(String.format("... with value [%s]", value));
					}

					propertyAccessor.setProperty(persistentProperty, value);
				}
				catch (Exception cause) {
					throw new MappingException(String.format(
						"While setting value [%1$s] of property [%2$s] for entity of type [%3$s] from PDX%4$s",
							value, persistentProperty.getName(), type, (customSerializer != null ?
								String.format(" using custom PdxSerializer [%14s]", customSerializer) : "")), cause);
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

					PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType());

					Object propertyValue = null;

					try {

						propertyValue = propertyAccessor.getProperty(persistentProperty);

						if (getLogger().isDebugEnabled()) {
							getLogger().debug(String.format("Serializing entity [%1$s] property [%2$s] value [%3$s] of type [%4$s] to PDX%5$s",
								entity.getType().getName(), persistentProperty.getName(), propertyValue,
								ObjectUtils.nullSafeClassName(propertyValue), (customSerializer != null
									? String.format(" using custom PdxSerializer [%s]", customSerializer) : "")));
						}

						if (customSerializer != null) {
							customSerializer.toData(propertyValue, writer);
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
							ObjectUtils.nullSafeClassName(propertyValue), (customSerializer != null
								? String.format(" using custom PdxSerializer [%1$s].",
									customSerializer.getClass().getName()) : "")), cause);
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
