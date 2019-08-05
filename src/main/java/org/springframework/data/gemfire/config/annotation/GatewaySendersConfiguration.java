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

import org.apache.geode.cache.wan.GatewaySender;

import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * Spring {@link Configuration} class used to construct, configure and initialize {@link GatewaySender} instances
 * in a Spring application context.
 *
 * @author Udo Kohlmeyer
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.core.annotation.AnnotationAttributes
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.EnableGatewaySender
 * @see org.springframework.data.gemfire.config.annotation.EnableGatewaySenders
 * @since 2.2.0
 */
@Configuration
public class GatewaySendersConfiguration extends GatewaySenderConfiguration {

	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableGatewaySenders.class;
	}

	@Override
	public void registerBeanDefinitions(AnnotationMetadata annotationMetadata, BeanDefinitionRegistry registry) {

		if (isAnnotationPresent(annotationMetadata)) {

			AnnotationAttributes parentGatewaySendersAnnotation = getAnnotationAttributes(annotationMetadata);

			registerGatewaySenders(parentGatewaySendersAnnotation, registry);
		}
	}

	private void registerGatewaySenders(AnnotationAttributes parentGatewaySendersAnnotation,
			BeanDefinitionRegistry registry) {

		AnnotationAttributes[] gatewaySenderAnnotations =
			parentGatewaySendersAnnotation.getAnnotationArray("gatewaySenders");

		if (ArrayUtils.isNotEmpty(gatewaySenderAnnotations)) {
			for (AnnotationAttributes gatewaySenderAnnotation : gatewaySenderAnnotations) {
				registerGatewaySender(gatewaySenderAnnotation, parentGatewaySendersAnnotation, registry);
			}
		}
		else {
			registerDefaultGatewaySender(parentGatewaySendersAnnotation, registry);
		}
	}

	private void registerDefaultGatewaySender(AnnotationAttributes parentGatewaySendersAnnotation,
			BeanDefinitionRegistry registry) {

		registerGatewaySender("GatewaySender", parentGatewaySendersAnnotation,
			parentGatewaySendersAnnotation, registry);
	}
}
