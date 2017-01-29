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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.util.Map;

import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;

/**
 * The {@link AddPoolsConfiguration} class registers {@link org.springframework.data.gemfire.client.PoolFactoryBean}
 * bean definitions for all {@link EnablePool} annotation configuration meta-data defined in
 * the {@link EnablePools} annotation on a GemFire client cache application class.

 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.data.gemfire.config.annotation.AddPoolConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnablePool
 * @see org.springframework.data.gemfire.config.annotation.EnablePools
 * @since 1.9.0
 */
public class AddPoolsConfiguration extends AddPoolConfiguration {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		if (importingClassMetadata.hasAnnotation(EnablePools.class.getName())) {
			Map<String, Object> enablePoolsAttributes = importingClassMetadata.getAnnotationAttributes(
				EnablePools.class.getName());

			AnnotationAttributes[] serversAttributes = (AnnotationAttributes[]) enablePoolsAttributes.get("pools");

			for (AnnotationAttributes enablePoolAttributes : serversAttributes) {
				registerPoolFactoryBeanDefinition(enablePoolAttributes, registry);
			}
		}
	}
}
