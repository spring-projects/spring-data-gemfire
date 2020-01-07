/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.serialization;

import org.apache.geode.DataSerializable;
import org.apache.geode.Instantiator;

/**
 * Factory that  generates {@link Instantiator} classes to improve instantiation of
 * custom types.
 *
 * @author Costin Leau
 */
public interface InstantiatorGenerator {

	/**
	 * Returns a (potentially new) Instantiator that optimizes the instantiation of the given types.
	 *
	 * @param clazz class produced by the instantiator
	 * @param classId instantiator class id
	 * @return an instantiator optimized for the given type.
	 */
	Instantiator getInstantiator(Class<? extends DataSerializable> clazz, int classId);
}
