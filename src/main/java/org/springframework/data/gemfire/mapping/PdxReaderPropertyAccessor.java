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

import org.apache.geode.pdx.PdxReader;

import org.springframework.expression.EvaluationContext;
import org.springframework.expression.PropertyAccessor;
import org.springframework.expression.TypedValue;

/**
 * {@link PropertyAccessor} to read values from a {@link PdxReader}.
 *
 * @author Oliver Gierke
 */
enum PdxReaderPropertyAccessor implements PropertyAccessor {

	INSTANCE;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.expression.PropertyAccessor#getSpecificTargetClasses()
	 */
	@Override
	public Class<?>[] getSpecificTargetClasses() {
		return new Class<?>[] { PdxReader.class };
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.expression.PropertyAccessor#canRead(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public boolean canRead(EvaluationContext evaluationContext, Object target, String name) {
		return ((PdxReader) target).hasField(name);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.expression.PropertyAccessor#read(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public TypedValue read(EvaluationContext evaluationContext, Object target, String name) {

		Object object = ((PdxReader) target).readObject(name);

		return object != null ? new TypedValue(object) : TypedValue.NULL;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.expression.PropertyAccessor#canWrite(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String)
	 */
	@Override
	public boolean canWrite(EvaluationContext evaluationContext, Object target, String name) {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.expression.PropertyAccessor#write(org.springframework.expression.EvaluationContext, java.lang.Object, java.lang.String, java.lang.Object)
	 */
	@Override
	public void write(EvaluationContext evaluationContext, Object target, String name, Object newValue) {
		throw new UnsupportedOperationException();
	}
}
