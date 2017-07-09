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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.pdx.PdxReader;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.pdx.PdxWriter;
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
 * @see org.springframework.data.mapping.PersistentEntity
 * @see org.springframework.data.mapping.PersistentPropertyAccessor
 * @see org.springframework.data.mapping.model.PersistentEntityParameterValueProvider
 * @see org.springframework.data.mapping.model.SpELContext
 * @see org.apache.geode.pdx.PdxReader
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.apache.geode.pdx.PdxWriter
 */
public class MappingPdxSerializer implements PdxSerializer, ApplicationContextAware {

	private final ConversionService conversionService;

	private EntityInstantiators instantiators;

	private final GemfireMappingContext mappingContext;

	protected final Log log = LogFactory.getLog(getClass());

	private Map<Class<?>, PdxSerializer> customSerializers;

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

		mappingContext = (mappingContext != null ? mappingContext : new GemfireMappingContext());
		conversionService = (conversionService != null ? conversionService : new DefaultConversionService());

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

		Assert.notNull(mappingContext, "GemfireMappingContext must not be null");
		Assert.notNull(conversionService, "ConversionService must not be null");

		this.mappingContext = mappingContext;
		this.conversionService = conversionService;
		this.instantiators = new EntityInstantiators();
		this.customSerializers = Collections.emptyMap();
		this.context = new SpELContext(PdxReaderPropertyAccessor.INSTANCE);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.context = new SpELContext(this.context, applicationContext);
	}

	/* (non-Javadoc) */
	protected ConversionService getConversionService() {
		return conversionService;
	}

	/**
	 * Configures custom PDX serializers to use for specific class types.
	 *
	 * @param customSerializers a mapping of domain object class types and their corresponding PDX serializer.
	 */
	public void setCustomSerializers(Map<Class<?>, PdxSerializer> customSerializers) {
		Assert.notNull(customSerializers, "Custom PdxSerializers must not be null");
		this.customSerializers = customSerializers;
	}

	/* (non-Javadoc) */
	protected Map<Class<?>, PdxSerializer> getCustomSerializers() {
		return Collections.unmodifiableMap(customSerializers);
	}

	/**
	 * Configures the {@link EntityInstantiator}s used to create the instances read by this PdxSerializer.
	 *
	 * @param gemfireInstantiators must not be {@literal null}.
	 */
	public void setGemfireInstantiators(Map<Class<?>, EntityInstantiator> gemfireInstantiators) {
		Assert.notNull(gemfireInstantiators, "EntityInstantiators must not be null");
		this.instantiators = new EntityInstantiators(gemfireInstantiators);
	}

	/* (non-Javadoc) */
	protected EntityInstantiators getGemfireInstantiators() {
		return instantiators;
	}

	/* (non-Javadoc) */
	protected GemfireMappingContext getMappingContext() {
		return mappingContext;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object fromData(Class<?> type, PdxReader reader) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(type);

		Object instance = getInstantiatorFor(entity).createInstance(entity,
			new PersistentEntityParameterValueProvider<>(entity, new GemfirePropertyValueProvider(reader), null));

		PersistentPropertyAccessor propertyAccessor =
			new ConvertingPropertyAccessor(entity.getPropertyAccessor(instance), getConversionService());

		entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

			if (!entity.isConstructorArgument(persistentProperty)) {

				PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType());

				Object value = null;

				try {
					if (log.isDebugEnabled()) {
						log.debug(String.format("Setting property [%1$s] for entity [%2$s] of type [%3$s] from PDX%4$s",
							persistentProperty.getName(), instance, type, (customSerializer != null ?
								String.format(" using custom PdxSerializer [%1$s]", customSerializer) : "")));
					}

					value = (customSerializer != null
						? customSerializer.fromData(persistentProperty.getType(), reader)
						: reader.readField(persistentProperty.getName()));

					if (log.isDebugEnabled()) {
						log.debug(String.format("... with value [%s]", value));
					}

					propertyAccessor.setProperty(persistentProperty, value);
				}
				catch (Exception e) {
					throw new MappingException(String.format(
						"While setting value [%1$s] of property [%2$s] for entity of type [%3$s] from PDX%4$s",
							value, persistentProperty.getName(), type, (customSerializer != null ?
								String.format(" using custom PdxSerializer [%14s]", customSerializer) : "")), e);
				}
			}
		});

		return propertyAccessor.getBean();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("unchecked")
	public boolean toData(Object value, PdxWriter writer) {

		GemfirePersistentEntity<?> entity = getPersistentEntity(value);

		PersistentPropertyAccessor propertyAccessor =
			new ConvertingPropertyAccessor(entity.getPropertyAccessor(value), getConversionService());

		entity.doWithProperties((PropertyHandler<GemfirePersistentProperty>) persistentProperty -> {

			PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType());

			Object propertyValue = null;

			try {
				propertyValue = propertyAccessor.getProperty(persistentProperty);

				if (log.isDebugEnabled()) {
					log.debug(String.format("Serializing entity property [%1$s] value [%2$s] of type [%3$s] to PDX%4$s",
						persistentProperty.getName(), propertyValue, value.getClass(), (customSerializer != null ?
							String.format(" using custom PdxSerializer [%s]", customSerializer) : "")));
				}

				if (customSerializer != null) {
					customSerializer.toData(propertyValue, writer);
				}
				else {
					writer.writeField(persistentProperty.getName(), propertyValue,
						(Class<Object>) persistentProperty.getType());
				}
			}
			catch (Exception e) {
				throw new MappingException(String.format(
					"While serializing entity property [%1$s] value [%2$s] of type [%3$s] to PDX%4$s",
						persistentProperty.getName(), propertyValue, value.getClass(),
							(customSerializer != null ? String.format(" using custom PdxSerializer [%1$s].",
								customSerializer.getClass().getName()) : ".")), e);
			}
		});

		GemfirePersistentProperty idProperty = entity.getIdProperty();

		if (idProperty != null) {
			writer.markIdentityField(idProperty.getName());
		}

		return true;
	}

	/**
	 * Looks up and returns a custom PdxSerializer based on the class type of the object to (de)serialize.
	 *
	 * @param type the Class type of the object to (de)serialize.
	 * @return a "custom" PdxSerializer for the given class type or null if no custom PdxSerializer
	 * for the given class type was registered.
	 * @see #getCustomSerializers()
	 * @see org.apache.geode.pdx.PdxSerializer
	 */
	protected PdxSerializer getCustomSerializer(Class<?> type) {
		return getCustomSerializers().get(type);
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
}
