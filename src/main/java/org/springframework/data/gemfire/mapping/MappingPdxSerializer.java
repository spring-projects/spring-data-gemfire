/*
 * Copyright 2012 the original author or authors.
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

import java.util.Map;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.convert.support.DefaultConversionService;
import org.springframework.data.convert.EntityInstantiator;
import org.springframework.data.convert.EntityInstantiators;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.mapping.PropertyHandler;
import org.springframework.data.mapping.model.BeanWrapper;
import org.springframework.data.mapping.model.MappingException;
import org.springframework.data.mapping.model.PersistentEntityParameterValueProvider;
import org.springframework.data.mapping.model.SpELContext;
import org.springframework.util.Assert;

import com.gemstone.gemfire.pdx.PdxReader;
import com.gemstone.gemfire.pdx.PdxSerializer;
import com.gemstone.gemfire.pdx.PdxWriter;

/**
 * {@link PdxSerializer} implementation that uses a
 * {@link GemfireMappingContext} to read and write entities.
 * 
 * @author Oliver Gierke
 * @author David Turanski
 */
public class MappingPdxSerializer implements PdxSerializer, ApplicationContextAware {

	private final GemfireMappingContext mappingContext;

	private final ConversionService conversionService;

	private EntityInstantiators instantiators;
	
	private Map<Class<?>,PdxSerializer> customSerializers;

	private SpELContext context;

	/**
	 * Creates a new {@link MappingPdxSerializer} using the given
	 * {@link GemfireMappingContext} and {@link ConversionService}.
	 * 
	 * @param mappingContext must not be {@literal null}.
	 * @param conversionService must not be {@literal null}.
	 */
	public MappingPdxSerializer(GemfireMappingContext mappingContext, ConversionService conversionService) {

		Assert.notNull(mappingContext);
		Assert.notNull(conversionService);

		this.mappingContext = mappingContext;
		this.conversionService = conversionService;
		this.instantiators = new EntityInstantiators();
		this.context = new SpELContext(PdxReaderPropertyAccessor.INSTANCE);
	}

	/**
	 * Creates a new {@link MappingPdxSerializer} using the default
	 * {@link GemfireMappingContext} and {@link DefaultConversionService}.
	 */
	public MappingPdxSerializer() {
		this(new GemfireMappingContext(), new DefaultConversionService());
	}

	/**
	 * Configures the {@link EntityInstantiator}s to be used to create the
	 * instances to be read.
	 * 
	 * @param gemfireInstantiators must not be {@literal null}.
	 */
	public void setGemfireInstantiators(Map<Class<?>, EntityInstantiator> gemfireInstantiators) {
		Assert.notNull(gemfireInstantiators);
		this.instantiators = new EntityInstantiators(gemfireInstantiators);
	}

	/**
	 * Configures custom pdx serializers to use for specific types 
	 * @param customSerializers
	 */
	public void setCustomSerializers(Map<Class<?>, PdxSerializer> customSerializers) {
		this.customSerializers = customSerializers;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.springframework.context.ApplicationContextAware#setApplicationContext
	 * (org.springframework.context.ApplicationContext)
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.context = new SpELContext(context, applicationContext);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.gemstone.gemfire.pdx.PdxSerializer#fromData(java.lang.Class,
	 * com.gemstone.gemfire.pdx.PdxReader)
	 */
	@Override
	public Object fromData(Class<?> type, final PdxReader reader) {
		final GemfirePersistentEntity<?> entity = mappingContext.getPersistentEntity(type);
		EntityInstantiator instantiator = instantiators.getInstantiatorFor(entity);
		GemfirePropertyValueProvider propertyValueProvider = new GemfirePropertyValueProvider(reader);

		PersistentEntityParameterValueProvider<GemfirePersistentProperty> provider = new PersistentEntityParameterValueProvider<GemfirePersistentProperty>(
				entity, propertyValueProvider, null);
 
		Object instance = instantiator.createInstance(entity, provider);

		final BeanWrapper<PersistentEntity<Object, ?>, Object> wrapper = BeanWrapper
				.create(instance, conversionService);

		entity.doWithProperties(new PropertyHandler<GemfirePersistentProperty>() {
			@Override
			public void doWithPersistentProperty(GemfirePersistentProperty persistentProperty) {

				if (entity.isConstructorArgument(persistentProperty)) {
					return;
				}
				
				PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType()); 
				Object value = null;
				if (customSerializer != null) {
					value = customSerializer.fromData(persistentProperty.getType(), reader);
				} else {
					value = reader.readField(persistentProperty.getName());
				}
				try {
					wrapper.setProperty(persistentProperty, value);
				}
				catch (Exception e) {
					throw new MappingException("Could not read value " + value.toString(), e);
				}
			}
		});

		return wrapper.getBean();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.gemstone.gemfire.pdx.PdxSerializer#toData(java.lang.Object,
	 * com.gemstone.gemfire.pdx.PdxWriter)
	 */
	@Override
	public boolean toData(Object value, final PdxWriter writer) {
		GemfirePersistentEntity<?> entity = mappingContext.getPersistentEntity(value.getClass());
		final BeanWrapper<PersistentEntity<Object, ?>, Object> wrapper = BeanWrapper.create(value, conversionService);

		entity.doWithProperties(new PropertyHandler<GemfirePersistentProperty>() {
			@SuppressWarnings({ "unchecked", "rawtypes" })
			@Override
			public void doWithPersistentProperty(GemfirePersistentProperty persistentProperty) {
				
				try {
					Object propertyValue = wrapper.getProperty(persistentProperty);
					PdxSerializer customSerializer = getCustomSerializer(persistentProperty.getType());
					if (customSerializer != null) {
						customSerializer.toData(propertyValue, writer);
					} else {
						writer.writeField(persistentProperty.getName(), propertyValue, (Class) persistentProperty.getType());
					} 
				}
				catch (Exception e) {
					throw new MappingException("Could not write value for property " + persistentProperty.toString(), e);
				}
			}
		});

		GemfirePersistentProperty idProperty = entity.getIdProperty();

		if (idProperty != null) {
			writer.markIdentityField(idProperty.getName());
		}

		return true;
	}
	
	private PdxSerializer getCustomSerializer(Class<?> clazz) {
		return customSerializers == null ? null : customSerializers.get(clazz);
	}
}
